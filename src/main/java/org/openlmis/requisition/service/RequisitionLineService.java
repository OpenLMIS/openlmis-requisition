package org.openlmis.requisition.service;

import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;


@Service
public class RequisitionLineService {

  @Autowired
  private RequisitionLineRepository requisitionLineRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private PeriodRepository periodRepository;

  public RequisitionLine save(RequisitionLine requisitionLine) {
    if (requisitionLine == null) {
      throw new RequisitionException("Requisition line does not exist");
    } else {
      RequisitionTemplate requisitionTemplate =
          requisitionTemplateRepository.findByProgram(
              requisitionLine.getRequisition().getProgram());

      RequisitionTemplateColumn requisitionTemplateColumn =
          requisitionTemplate.getColumnsMap().get("beginningBalance");

      if (!requisitionTemplateColumn.getCanBeChangedByUser()) {
        resetBeginningBalance(requisitionLine);
      }
      requisitionLineRepository.save(requisitionLine);
      return requisitionLine;
    }
  }


  public Requisition initiateRequisitionLineFields(Requisition requisition) {
    RequisitionTemplate requisitionTemplate =
        requisitionTemplateRepository.findByProgram(requisition.getProgram());

    initiateBeginningBalance(requisition, requisitionTemplate);

    return requisition;
  }

  private void initiateBeginningBalance(Requisition requisition,
                                        RequisitionTemplate requisitionTemplate) {
    Iterable<Period> previousPeriods = periodRepository.findPreviousPeriods(
        requisition.getProcessingPeriod().getProcessingSchedule(),
        requisition.getProcessingPeriod().getStartDate());

    if (requisitionTemplate.getColumnsMap().get("beginningBalance").getIsDisplayed()
        && previousPeriods.iterator().hasNext()) {

      Requisition previousRequisition;
      RequisitionLine previousRequisitionLine;
      previousRequisition = requisitionRepository.findByProcessingPeriod(
          previousPeriods.iterator().next());

      for (RequisitionLine requisitionLine : requisition.getRequisitionLines()) {
        previousRequisitionLine = requisitionLineRepository.findByRequisitionAndProduct(
            previousRequisition, requisitionLine.getProduct());

        if (requisitionLine.getBeginningBalance() == null) {
          if (previousRequisitionLine != null
              && previousRequisitionLine.getStockInHand() != null) {

            requisitionLine.setBeginningBalance(previousRequisitionLine.getStockInHand());
          } else if (requisitionLine.getStockInHand() == null) {
            requisitionLine.setBeginningBalance(0);
          }
        }
      }

    } else {
      for (RequisitionLine requisitionLine : requisition.getRequisitionLines()) {
        requisitionLine.setBeginningBalance(0);
      }
    }
  }

  private void resetBeginningBalance(RequisitionLine requisitionLine) {
    Iterable<Period> previousPeriods = periodRepository.findPreviousPeriods(
        requisitionLine.getRequisition().getProcessingPeriod().getProcessingSchedule(),
        requisitionLine.getRequisition().getProcessingPeriod().getStartDate());

    if (!previousPeriods.iterator().hasNext()) {
      requisitionLine.setBeginningBalance(0);
      return;
    }

    Requisition previousRequisition;
    previousRequisition = requisitionRepository.findByProcessingPeriod(
        previousPeriods.iterator().next());

    if (previousRequisition == null) {
      requisitionLine.setBeginningBalance(0);
      return;
    }

    RequisitionLine previousRequisitionLine;
    previousRequisitionLine = requisitionLineRepository.findByRequisitionAndProduct(
        previousRequisition, requisitionLine.getProduct());

    if (previousRequisitionLine == null) {
      requisitionLine.setBeginningBalance(0);
      return;
    }

    if (requisitionLine.getBeginningBalance() != previousRequisitionLine.getStockInHand()) {
      requisitionLine.setBeginningBalance(previousRequisitionLine.getStockInHand());
    }

  }

}
