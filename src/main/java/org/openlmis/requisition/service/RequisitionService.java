package org.openlmis.requisition.service;

import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

@Service
public class RequisitionService {

  Logger logger = LoggerFactory.getLogger(RequisitionService.class);

  @Autowired
  RequisitionRepository requisitionRepository;

  @PersistenceContext
  EntityManager entityManager;

  public boolean skip(Requisition requisition) {

    if (requisition == null) {
      logger.debug("Skip failed - requisition cannot be null");
    } else if (!requisition.getStatus().equals(RequisitionStatus.INITIATED)) {
      logger.debug("Skip failed - requisition has bad status");
    } else if (!requisition.getProgram().getSkippable()) {
      logger.debug("Skip failed - requisition program does not allow skipping");
    } else {
      logger.debug("Requisition skipped");
      requisition.setStatus(RequisitionStatus.SKIPPED);
      requisitionRepository.save(requisition);
      return true;
    }
    return false;
  }

  /**
   * Finds requisitions matching all of provided parameters.
   */
  public List<Requisition> searchRequisitions(Facility facility, Program program,
                                              LocalDateTime createdDateFrom,
                                              LocalDateTime createdDateTo) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Requisition> query = builder.createQuery(Requisition.class);
    Root<Requisition> root = query.from(Requisition.class);

    Predicate predicate = builder.conjunction();
    if (facility != null) {
      predicate = builder.and(predicate, builder.equal(root.get("facility"), facility));
    }
    if (program != null) {
      predicate = builder.and(predicate, builder.equal(root.get("program"), program));
    }
    if (createdDateFrom != null) {
      predicate = builder.and(predicate,
          builder.greaterThanOrEqualTo(root.get("createdDate"), createdDateFrom));
    }
    if (createdDateTo != null) {
      predicate = builder.and(predicate,
          builder.lessThanOrEqualTo(root.get("createdDate"), createdDateTo));
    }

    query.where(predicate);
    return entityManager.createQuery(query).getResultList();
  }
}
