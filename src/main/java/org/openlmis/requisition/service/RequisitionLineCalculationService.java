package org.openlmis.requisition.service;

import static java.util.stream.Collectors.toList;
import static org.apache.commons.lang.BooleanUtils.isTrue;

import org.apache.commons.lang3.ObjectUtils;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.StockAdjustment;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.dto.StockAdjustmentReasonDto;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.openlmis.requisition.service.referencedata.OrderableProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.StockAdjustmentReasonReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.UUID;

@Service
public class RequisitionLineCalculationService {
  private static final String BEGINNING_BALANCE_COLUMN = "beginningBalance";

  @Autowired
  private RequisitionService requisitionService;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private OrderableProductReferenceDataService orderableProductReferenceDataService;

  @Autowired
  private StockAdjustmentReasonReferenceDataService stockAdjustmentReasonReferenceDataService;

  /**
   * Initiate all RequisitionLineItem fields from given Requisition to default value.
   *
   * @param requisition Requisition with RequisitionLineItems to be initiated.
   * @return Returns Requisition with initiated RequisitionLineItems.
   */
  public Requisition initiateRequisitionLineItemFields(Requisition requisition,
                                                       RequisitionTemplate requisitionTemplate)
      throws RequisitionTemplateColumnException {
    initiateBeginningBalance(requisition, requisitionTemplate);
    initiateTotalQuantityReceived(requisition);

    return requisition;
  }

  private void initiateBeginningBalance(Requisition requisition, RequisitionTemplate template)
      throws RequisitionTemplateColumnException {

    if (template.isColumnDisplayed(BEGINNING_BALANCE_COLUMN)) {
      ProcessingPeriodDto previousPeriod = findPreviousPeriod(requisition.getProcessingPeriodId());
      Requisition previousRequisition = null != previousPeriod
          ? findPreviousRequisition(requisition, previousPeriod)
          : null;

      if (null != previousRequisition) {
        requisition.forEachLine(currentLine -> {
          RequisitionLineItem previousLine = previousRequisition
              .findLineByProductId(currentLine.getOrderableProductId());

          if (null != previousLine) {
            currentLine.setBeginningBalance(previousLine.getStockOnHand());
          }
        });
      }
    }

    requisition.forEachLine(line -> {
      if (null == line.getBeginningBalance()) {
        line.setBeginningBalance(0);
      }
    });
  }

  private ProcessingPeriodDto findPreviousPeriod(UUID periodId) {
    // retrieve data from reference-data
    ProcessingPeriodDto period = periodReferenceDataService.findOne(periodId);

    if (null == period) {
      return null;
    }

    Collection<ProcessingPeriodDto> collection = periodReferenceDataService
        .search(period.getProcessingSchedule().getId(), period.getStartDate());

    if (null == collection || collection.isEmpty()) {
      return null;
    }

    // create a list...
    List<ProcessingPeriodDto> list = new ArrayList<>(collection);
    // ...remove the latest period from the list because it is not previous...
    list.removeIf(p -> p.getId().equals(periodId));
    // .. and sort elements by startDate property DESC.
    list.sort((one, two) -> ObjectUtils.compare(two.getStartDate(), one.getStartDate()));

    // The latest previous date should be first.
    return list.isEmpty() ? null : list.get(0);
  }

  private Requisition findPreviousRequisition(Requisition requisition, ProcessingPeriodDto period) {
    List<Requisition> list = requisitionService.searchRequisitions(
        requisition.getFacilityId(), requisition.getProgramId(),
        null, null, period.getId(), null, null
    );

    return null == list ? null : (list.isEmpty() ? null : list.get(0));
  }

  private void initiateTotalQuantityReceived(Requisition requisition) {
    for (RequisitionLineItem requisitionLineItem : requisition.getRequisitionLineItems()) {
      requisitionLineItem.setTotalReceivedQuantity(0);
    }
  }

  /**
   * Return list of RequisitionLineItemDtos for a given RequisitionLineItem.
   *
   * @param requisitionLineItems List of RequisitionLineItems to be exported to Dto
   * @return list of RequisitionLineItemDtos
   */
  public List<RequisitionLineItemDto> exportToDtos(
      List<RequisitionLineItem> requisitionLineItems) {
    return requisitionLineItems.stream().map(this::exportToDto).collect(toList());
  }

  private RequisitionLineItemDto exportToDto(RequisitionLineItem requisitionLineItem) {
    RequisitionLineItemDto dto = new RequisitionLineItemDto();
    requisitionLineItem.export(dto);
    dto.setOrderableProduct(orderableProductReferenceDataService.findOne(
        requisitionLineItem.getOrderableProductId()));
    return dto;
  }

  /**
   * Calculates TotalLossesAndAdjustments (D) value for each line in the given requisition.
   * The property is calculated by taking all item's StockAdjustments and adding their quantities.
   * Values, whose StockAdjustmentReasons are additive, count as positive, and negative otherwise.
   */
  public void calculateTotalLossesAndAdjustments(Requisition requisition) {
    requisition.forEachLine(line -> {
      List<StockAdjustment> adjustments = line.getStockAdjustments();

      if (null != adjustments) {
        int total = 0;

        for (StockAdjustment adjustment : adjustments) {
          StockAdjustmentReasonDto reason =
              stockAdjustmentReasonReferenceDataService.findOne(adjustment.getReasonId());
          int sign = isTrue(reason.getAdditive()) ? 1 : -1;

          total += adjustment.getQuantity() * sign;
        }

        line.setTotalLossesAndAdjustments(total);
      }
    });
  }
}
