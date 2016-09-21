package org.openlmis.requisition.service;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionLineItemRepository;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
public class RequisitionLineItemService {

  @Autowired
  private RequisitionLineItemRepository requisitionLineItemRepository;

  @Autowired
  private RequisitionService requisitionService;

  @Autowired
  private RequisitionTemplateService requisitionTemplateService;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;


  /**
   * Saves given RequisitionLineItem if possible.
   *
   * @param requisition Requisition which contains given RequisitionLineItem.
   * @param requisitionLineItem Requisition Line to be saved.
   * @return Saved RequisitionLineItem.
   * @throws RequisitionException Exception thrown when
   *      it is not possible to save given RequisitionLineItem.
   */
  public RequisitionLineItem save(Requisition requisition,
                                  RequisitionLineItem requisitionLineItem)
      throws RequisitionException {
    if (requisitionLineItem == null) {
      throw new RequisitionException("Requisition line does not exist");
    } else {

      List<RequisitionTemplate> requisitionTemplateList = requisitionTemplateService
          .searchRequisitionTemplates(requisition.getProgram());

      RequisitionTemplateColumn requisitionTemplateColumn =
          requisitionTemplateList.get(0).getColumnsMap().get("beginningBalance");

      if (!requisitionTemplateColumn.getCanBeChangedByUser()) {
        resetBeginningBalance(requisition, requisitionLineItem);
      }

      requisitionLineItemRepository.save(requisitionLineItem);
      return requisitionLineItem;
    }
  }

  /**
   * Method returns all requisition lines with matched parameters.
   * @param requisition requisition of searched requisition lines.
   * @param product product of searched requisition lines.
   * @return list of requisition lines with matched parameters.
   */
  public List<RequisitionLineItem> searchRequisitionLineItems(
      Requisition requisition, UUID product) {
    return requisitionLineItemRepository.searchRequisitionLineItems(requisition, product);
  }

  /**
   * Initiate all RequisitionLineItem fields from given Requisition to default value.
   *
   * @param requisition Requisition with RequisitionLineItems to be initiated.
   * @return Returns Requisition with initiated RequisitionLineItems.
   */
  public Requisition initiateRequisitionLineItemFields(Requisition requisition) {
    List<RequisitionTemplate> requisitionTemplateList
        = requisitionTemplateService.searchRequisitionTemplates(requisition.getProgram());

    if (!requisitionTemplateList.isEmpty()) {
      initiateBeginningBalance(requisition, requisitionTemplateList.get(0));
      initiateTotalQuantityReceived(requisition);
    }

    return requisition;
  }

  private void initiateBeginningBalance(Requisition requisition,
                                        RequisitionTemplate requisitionTemplate) {

    ProcessingPeriodDto period = periodReferenceDataService.findOne(
            requisition.getProcessingPeriod());

    Iterable<ProcessingPeriodDto> previousPeriods = periodReferenceDataService.search(
            period.getProcessingSchedule(),
            period.getStartDate());

    if (requisitionTemplate.getColumnsMap().get("beginningBalance").getIsDisplayed()
        && previousPeriods != null && previousPeriods.iterator().hasNext()) {

      List<Requisition> previousRequisition;
      List<RequisitionLineItem> previousRequisitionLineItem;

      previousRequisition = requisitionService.searchRequisitions(
              requisition.getFacility(),
              requisition.getProgram(),
              null,null,
              previousPeriods.iterator().next().getId(),
              null,
              null);
      if (previousRequisition.size() == 0) {
        return;
      }
      for (RequisitionLineItem requisitionLineItem : requisition.getRequisitionLineItems()) {
        previousRequisitionLineItem = searchRequisitionLineItems(
            previousRequisition.get(0), requisitionLineItem.getProduct());

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

  private void resetBeginningBalance(Requisition requisition,
                                     RequisitionLineItem requisitionLineItem) {
    ProcessingPeriodDto period = periodReferenceDataService.findOne(
            requisitionLineItem.getRequisition().getProcessingPeriod());
    Iterable<ProcessingPeriodDto> previousPeriods = periodReferenceDataService.search(
        period.getProcessingSchedule(),
        period.getStartDate());

    if (!previousPeriods.iterator().hasNext()) {
      requisitionLineItem.setBeginningBalance(0);
      return;
    }

    List<Requisition> previousRequisition =
            requisitionService.searchRequisitions(
                requisition.getFacility(),
                requisition.getProgram(),
                null,
                null,
                previousPeriods.iterator().next().getId(),
                null,
                null);

    if (previousRequisition.size() == 0) {
      requisitionLineItem.setBeginningBalance(0);
      return;
    }

    List<RequisitionLineItem> previousRequisitionLineItem;
    previousRequisitionLineItem = searchRequisitionLineItems(
        previousRequisition.get(0), requisitionLineItem.getProduct());

    if (previousRequisitionLineItem == null) {
      requisitionLineItem.setBeginningBalance(0);
      return;
    }

    if (requisitionLineItem.getBeginningBalance()
        != previousRequisitionLineItem.get(0).getStockInHand()) {
      requisitionLineItem.setBeginningBalance(previousRequisitionLineItem.get(0).getStockInHand());
    }

  }

  private void initiateTotalQuantityReceived(Requisition requisition) {
    for (RequisitionLineItem requisitionLineItem : requisition.getRequisitionLineItems()) {
      requisitionLineItem.setTotalReceivedQuantity(0);
    }
  }
}
