package org.openlmis.requisition.service;

import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.service.PeriodService;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

@Service
public class RequisitionLineService {

  @Autowired
  private RequisitionLineRepository requisitionLineRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private PeriodService periodService;

  @PersistenceContext
  private EntityManager entityManager;

  @Autowired
  private RequisitionTemplateService requisitionTemplateService;

  /**
   * Saves given RequisitionLine if possible
   *
   * @param requisition Requisition which contains given RequisitionLine.
   * @param requisitionLine Requisition Line to be saved.
   * @return Saved RequisitionLine.
   * @throws RequisitionException Exception thrown when
   *      it is not possible to save given RequisitionLine.
   */
  public RequisitionLine save(Requisition requisition,
                              RequisitionLine requisitionLine) throws RequisitionException {
    if (requisitionLine == null) {
      throw new RequisitionException("Requisition line does not exist");
    } else {
      List<RequisitionTemplate> requisitionTemplateList = requisitionTemplateService
          .searchRequisitionTemplates(requisitionLine.getRequisition().getProgram());

      RequisitionTemplateColumn requisitionTemplateColumn =
          requisitionTemplateList.get(0).getColumnsMap().get("beginningBalance");

      if (!requisitionTemplateColumn.getCanBeChangedByUser()) {
        resetBeginningBalance(requisition, requisitionLine);
      }

      requisitionLineRepository.save(requisitionLine);
      return requisitionLine;
    }
  }

  /**
   * Method returns all requisition lines with matched parameters.
   */
  public List<RequisitionLine> searchRequisitionLines(Requisition requisition, Product product) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<RequisitionLine> query = builder.createQuery(RequisitionLine.class);
    Root<RequisitionLine> root = query.from(RequisitionLine.class);
    Predicate predicate = builder.conjunction();

    if (requisition != null) {
      predicate = builder.and(
          predicate,
          builder.equal(
              root.get("requisition"), requisition));
    }
    if (product != null) {
      predicate = builder.and(
          predicate,
          builder.equal(
              root.get("product"), product));
    }

    query.where(predicate);
    return entityManager.createQuery(query).getResultList();
  }

  /**
   * Initiate all RequisitionLine fields from given Requisition to default value.
   *
   * @param requisition Requisition with RequisitionLines to be initiated.
   * @return Returns Requisition with initiated RequisitionLines.
   */
  public Requisition initiateRequisitionLineFields(Requisition requisition) {
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
    Iterable<Period> previousPeriods = periodService.searchPeriods(
        requisition.getProcessingPeriod().getProcessingSchedule(),
        requisition.getProcessingPeriod().getStartDate());

    if (requisitionTemplate.getColumnsMap().get("beginningBalance").getIsDisplayed()
        && previousPeriods != null && previousPeriods.iterator().hasNext()) {

      Requisition previousRequisition;
      List<RequisitionLine> previousRequisitionLine;
      previousRequisition = requisitionRepository.findByProcessingPeriodAndFacilityAndProgram(
          previousPeriods.iterator().next(), requisition.getFacility(), requisition.getProgram());

      for (RequisitionLine requisitionLine : requisition.getRequisitionLines()) {
        previousRequisitionLine = searchRequisitionLines(
            previousRequisition, requisitionLine.getProduct());

        if (requisitionLine.getBeginningBalance() == null) {
          if (previousRequisitionLine != null
              && previousRequisitionLine.get(0).getStockInHand() != null) {

            requisitionLine.setBeginningBalance(previousRequisitionLine.get(0).getStockInHand());
          } else {
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

  private void resetBeginningBalance(Requisition requisition, RequisitionLine requisitionLine) {
    Iterable<Period> previousPeriods = periodService.searchPeriods(
        requisitionLine.getRequisition().getProcessingPeriod().getProcessingSchedule(),
        requisitionLine.getRequisition().getProcessingPeriod().getStartDate());

    if (!previousPeriods.iterator().hasNext()) {
      requisitionLine.setBeginningBalance(0);
      return;
    }
    Requisition previousRequisition =
        requisitionRepository.findByProcessingPeriodAndFacilityAndProgram(
        previousPeriods.iterator().next(), requisition.getFacility(), requisition.getProgram()
        );

    if (previousRequisition == null) {
      requisitionLine.setBeginningBalance(0);
      return;
    }

    List<RequisitionLine> previousRequisitionLine;
    previousRequisitionLine = searchRequisitionLines(
        previousRequisition, requisitionLine.getProduct());

    if (previousRequisitionLine == null) {
      requisitionLine.setBeginningBalance(0);
      return;
    }

    if (requisitionLine.getBeginningBalance() != previousRequisitionLine.get(0).getStockInHand()) {
      requisitionLine.setBeginningBalance(previousRequisitionLine.get(0).getStockInHand());
    }

  }

  private void initiateTotalQuantityReceived(Requisition requisition) {
    for (RequisitionLine requisitionLine : requisition.getRequisitionLines()) {
      requisitionLine.setTotalReceivedQuantity(0);
    }
  }
}
