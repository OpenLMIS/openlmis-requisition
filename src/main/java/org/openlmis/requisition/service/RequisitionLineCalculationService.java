package org.openlmis.requisition.service;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

@Service
public class RequisitionLineCalculationService {
  private static final String BEGINNING_BALANCE_COLUMN = "beginningBalance";

  @Autowired
  private RequisitionService requisitionService;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;


  /**
   * Method returns all requisition lines with matched parameters.
   *
   * @param requisition requisition of searched requisition lines.
   * @param product     product of searched requisition lines.
   * @return list of requisition lines with matched parameters.
   */
  public List<RequisitionLineItem> searchRequisitionLineItems(
      Requisition requisition, UUID product) {
    List<RequisitionLineItem> items = new ArrayList<>();
    for (RequisitionLineItem item : requisition.getRequisitionLineItems()) {
      if (Objects.equals(product, item.getOrderableProduct())) {
        items.add(item);
      }
    }
    return items;
  }

  /**
   * Initiate all RequisitionLineItem fields from given Requisition to default value.
   *
   * @param requisition Requisition with RequisitionLineItems to be initiated.
   * @return Returns Requisition with initiated RequisitionLineItems.
   */
  public Requisition initiateRequisitionLineItemFields(Requisition requisition,
                                                       RequisitionTemplate requisitionTemplate) {
    initiateBeginningBalance(requisition, requisitionTemplate);
    initiateTotalQuantityReceived(requisition);

    return requisition;
  }

  private void initiateBeginningBalance(Requisition requisition,
                                        RequisitionTemplate requisitionTemplate) {

    ProcessingPeriodDto period = periodReferenceDataService.findOne(
        requisition.getProcessingPeriod());

    ProcessingScheduleDto schedule = period.getProcessingSchedule();
    Iterable<ProcessingPeriodDto> previousPeriods = periodReferenceDataService.search(
        schedule.getId(),
        period.getStartDate());

    if (requisitionTemplate.getColumnsMap().get(BEGINNING_BALANCE_COLUMN).getIsDisplayed()
        && previousPeriods != null && previousPeriods.iterator().hasNext()) {

      List<Requisition> previousRequisition;
      List<RequisitionLineItem> previousRequisitionLineItem;

      previousRequisition = requisitionService.searchRequisitions(
          requisition.getFacility(),
          requisition.getProgram(),
          null, null,
          previousPeriods.iterator().next().getId(),
          null,
          null);
      if (previousRequisition.size() == 0) {
        return;
      }
      for (RequisitionLineItem requisitionLineItem : requisition.getRequisitionLineItems()) {
        previousRequisitionLineItem = searchRequisitionLineItems(
            previousRequisition.get(0), requisitionLineItem.getOrderableProduct());

        if (requisitionLineItem.getBeginningBalance() == null) {
          if (previousRequisitionLineItem != null
              && previousRequisitionLineItem.get(0).getStockInHand() != null) {
            requisitionLineItem.setBeginningBalance(
                previousRequisitionLineItem.get(0).getStockInHand());
          } else {
            requisitionLineItem.setBeginningBalance(0);
          }
        }
      }
    } else {
      for (RequisitionLineItem requisitionLineItem : requisition.getRequisitionLineItems()) {
        requisitionLineItem.setBeginningBalance(0);
      }
    }
  }

  private void initiateTotalQuantityReceived(Requisition requisition) {
    for (RequisitionLineItem requisitionLineItem : requisition.getRequisitionLineItems()) {
      requisitionLineItem.setTotalReceivedQuantity(0);
    }
  }
}
