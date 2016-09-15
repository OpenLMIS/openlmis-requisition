package org.openlmis.requisition.repository.custom.impl;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.repository.custom.RequisitionRepositoryCustom;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.time.LocalDateTime;
import java.util.List;

public class RequisitionRepositoryImpl implements RequisitionRepositoryCustom {

  @PersistenceContext
  private EntityManager entityManager;

  /**
   * Method returns all Requisitions with matched parameters.
   * @param facility facility of searched Requisitions.
   * @param program program of searched Requisitions.
   * @param createdDateFrom After what date should searched Requisition be created.
   * @param createdDateTo Before what date should searched Requisition be created.
   * @param processingPeriod processingPeriod of searched Requisitions.
   * @param supervisoryNode supervisoryNode of searched Requisitions.
   * @param requisitionStatus status of searched Requisitions.
   * @return list of Requisitions with matched parameters.
   */
  public List<Requisition> searchRequisitions(FacilityDto facility, ProgramDto program,
                                              LocalDateTime createdDateFrom,
                                              LocalDateTime createdDateTo,
                                              ProcessingPeriodDto processingPeriod,
                                              SupervisoryNodeDto supervisoryNode,
                                              RequisitionStatus requisitionStatus) {
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
    if (processingPeriod != null) {
      predicate = builder.and(predicate,
              builder.equal(root.get("processingPeriod"), processingPeriod));
    }
    if (supervisoryNode != null) {
      predicate = builder.and(predicate,
              builder.equal(root.get("supervisoryNode"), supervisoryNode));
    }
    if (requisitionStatus != null) {
      predicate = builder.and(predicate,
              builder.equal(root.get("status"), requisitionStatus));
    }

    query.where(predicate);
    return entityManager.createQuery(query).getResultList();
  }
}
