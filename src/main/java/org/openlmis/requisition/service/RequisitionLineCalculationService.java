package org.openlmis.requisition.service;

import static java.util.stream.Collectors.toList;

import org.openlmis.requisition.domain.LineItemFieldsCalculator;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.openlmis.requisition.service.referencedata.OrderableProductReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class RequisitionLineCalculationService {
  private static final String BEGINNING_BALANCE_COLUMN = "beginningBalance";

  @Autowired
  private RequisitionService requisitionService;

  @Autowired
  private PeriodService periodService;

  @Autowired
  private OrderableProductReferenceDataService orderableProductReferenceDataService;

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

  /**
   * Calculates Packs to Ship for all RequisitionLineItems in this Requisition.
   */
  public void calculatePacksToShip(Requisition requisition) {
    requisition.forEachLine(line -> calculatePacksToShip(line));
  }

  /**
   * Calculates Packs to Ship (V) value.
   */
  private void calculatePacksToShip(RequisitionLineItem lineItem) {
    OrderableProductDto orderableProductDto =
        orderableProductReferenceDataService.findOne(lineItem.getOrderableProductId());

    Integer orderQuantity = getOrderQuantity(lineItem);
    long packSize = orderableProductDto.getPackSize();
    long packRoundingThreshold = orderableProductDto.getPackRoundingThreshold();
    boolean roundToZero = orderableProductDto.isRoundToZero();

    if (orderQuantity != null && orderQuantity == 0) {
      lineItem.setPacksToShip(0L);
    } else if (orderQuantity != null && packSize != 0) {
      long packsToShip = orderQuantity / packSize;
      long remainderQuantity = orderQuantity % packSize;

      if (remainderQuantity >= packRoundingThreshold) {
        packsToShip += 1;
      }

      if (packsToShip == 0 && !roundToZero) {
        packsToShip = 1;
      }

      lineItem.setPacksToShip(packsToShip);
    }
  }

  private Integer getOrderQuantity(RequisitionLineItem lineItem) {
    if (lineItem.getApprovedQuantity() != null) {
      return lineItem.getApprovedQuantity();
    }

    if (lineItem.getRequestedQuantity() != null) {
      return lineItem.getRequestedQuantity();
    }

    return 0;
  }

  private void initiateBeginningBalance(Requisition requisition, RequisitionTemplate template)
      throws RequisitionTemplateColumnException {

    // Firstly, we set the Beginning Balance to zero for all lines.
    requisition.forEachLine(line -> {
      if (null == line.getBeginningBalance()) {
        line.setBeginningBalance(0);
      }
    });

    // Secondly, if we display the column ...
    if (template.isColumnDisplayed(BEGINNING_BALANCE_COLUMN)) {
      // ... we try to find previous period and requisition ...
      ProcessingPeriodDto previousPeriod = periodService.findPreviousPeriod(
          requisition.getProcessingPeriodId()
      );
      Requisition previousRequisition = null != previousPeriod
          ? findPreviousRequisition(requisition, previousPeriod)
          : null;

      // ... and if the previous requisition exists ...
      if (null != previousRequisition) {
        // .. for each line from the current requsition ...
        requisition.forEachLine(currentLine -> {
          // ... we try to find line in the previous requisition for the same product ...
          RequisitionLineItem previousLine = previousRequisition
              .findLineByProductId(currentLine.getOrderableProductId());

          // ... and in the end we use it to calculate beginning balance in a new line.
          currentLine.setBeginningBalance(
              LineItemFieldsCalculator.calculateBeginningBalance(previousLine));
        });
      }
    }
  }

  private Requisition findPreviousRequisition(Requisition requisition, ProcessingPeriodDto period) {
    List<Requisition> list = requisitionService.searchRequisitions(
        requisition.getFacilityId(), requisition.getProgramId(),
        null, null, period.getId(), null, null, null
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

}
