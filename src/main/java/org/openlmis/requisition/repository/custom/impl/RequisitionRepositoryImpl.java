package org.openlmis.requisition.repository.custom.impl;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.custom.RequisitionRepositoryCustom;
import org.openlmis.utils.Pagination;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.time.ZonedDateTime;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

public class RequisitionRepositoryImpl implements RequisitionRepositoryCustom {

  private static final String FACILITY_ID = "facilityId";
  private static final String PROGRAM_ID = "programId";
  private static final String EMERGENCY = "emergency";
  private static final String STATUS = "status";
  private static final String CREATED_DATE = "createdDate";
  private static final String PROCESSING_PERIOD_ID = "processingPeriodId";
  private static final String SUPERVISORY_NODE_ID = "supervisoryNodeId";

  @PersistenceContext
  private EntityManager entityManager;

  /**
   * Method returns all Requisitions with matched parameters.
   *
   * @param facility            Facility of searched Requisitions.
   * @param program             Program of searched Requisitions.
   * @param createdDateFrom     After what date should searched Requisition be created.
   * @param createdDateTo       Before what date should searched Requisition be created.
   * @param processingPeriod    ProcessingPeriod of searched Requisitions.
   * @param supervisoryNode     SupervisoryNode of searched Requisitions.
   * @param requisitionStatuses Statuses of searched Requisitions.
   * @return List of Requisitions with matched parameters.
   */
  @Override
  public Page<Requisition> searchRequisitions(UUID facility, UUID program,
                                              ZonedDateTime createdDateFrom,
                                              ZonedDateTime createdDateTo,
                                              UUID processingPeriod,
                                              UUID supervisoryNode,
                                              Set<RequisitionStatus> requisitionStatuses,
                                              Boolean emergency,
                                              Pageable pageable) {
    //Retrieve a paginated set of results
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();

    CriteriaQuery<Requisition> queryMain = builder.createQuery(Requisition.class);
    Root<Requisition> root = queryMain.from(Requisition.class);

    Predicate predicate = builder.conjunction();
    if (facility != null) {
      predicate = builder.and(predicate, builder.equal(root.get(FACILITY_ID), facility));
    }
    if (program != null) {
      predicate = builder.and(predicate, builder.equal(root.get(PROGRAM_ID), program));
    }
    if (createdDateFrom != null) {
      predicate = builder.and(predicate,
          builder.greaterThanOrEqualTo(root.get(CREATED_DATE), createdDateFrom));
    }
    if (createdDateTo != null) {
      predicate = builder.and(predicate,
          builder.lessThanOrEqualTo(root.get(CREATED_DATE), createdDateTo));
    }
    if (processingPeriod != null) {
      predicate = builder.and(predicate,
          builder.equal(root.get(PROCESSING_PERIOD_ID), processingPeriod));
    }
    if (supervisoryNode != null) {
      predicate = builder.and(predicate,
          builder.equal(root.get(SUPERVISORY_NODE_ID), supervisoryNode));
    }
    predicate = filterByStatuses(builder, predicate, requisitionStatuses, root);
    if (null != emergency) {
      predicate = builder.and(predicate,
          builder.equal(root.get(EMERGENCY), emergency));
    }

    queryMain.where(predicate);

    int pageNumber = Pagination.getPageNumber(pageable);
    int pageSize = Pagination.getPageSize(pageable);

    List<Requisition> results = entityManager.createQuery(queryMain)
                                .setFirstResult(pageNumber * pageSize)
                                .setMaxResults(pageSize)
                                .getResultList();

    //Having retrieved just paginated values we care about, determine
    //the total number of values in the system which meet our criteria.
    CriteriaQuery<Long> queryCount = builder.createQuery(Long.class);
    Root<Requisition> rootQueryCount = queryCount.from(Requisition.class);
    queryCount.select(builder.count(rootQueryCount));
    queryCount.where(predicate);
    Long count = entityManager.createQuery(queryCount).getSingleResult();

    return Pagination.getPage(results, pageable, count);
  }


  /**
   * Method returns all Requisitions with matched parameters.
   *
   * @param processingPeriod ProcessingPeriod of searched Requisitions.
   * @param emergency        if {@code true}, the method will look only for emergency requisitions,
   *                         if {@code false}, the method will look only for standard requisitions,
   *                         if {@code null} the method will check all requisitions.
   * @return List of Requisitions with matched parameters.
   */
  @Override
  public List<Requisition> searchRequisitions(UUID processingPeriod,
                                              UUID facility,
                                              UUID program,
                                              Boolean emergency) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Requisition> query = builder.createQuery(Requisition.class);
    Root<Requisition> root = query.from(Requisition.class);
    Predicate predicate = builder.conjunction();

    if (null != emergency) {
      predicate = builder.and(predicate, builder.equal(root.get(EMERGENCY), emergency));
    }
    if (processingPeriod != null) {
      predicate = builder.and(predicate,
          builder.equal(root.get(PROCESSING_PERIOD_ID), processingPeriod));
    }
    if (facility != null) {
      predicate = builder.and(predicate,
          builder.equal(root.get(FACILITY_ID), facility));
    }
    if (program != null) {
      predicate = builder.and(predicate,
          builder.equal(root.get(PROGRAM_ID), program));
    }
    query.where(predicate);
    return entityManager.createQuery(query).getResultList();
  }

  /**
   * Get approved requisitions matching all of provided parameters.
   *
   * @param filterBy     Field used to filter: "programName","facilityCode","facilityName" or
   *                     "all".
   * @param desiredUuids Desired UUID list.
   * @return List of requisitions.
   */
  @Override
  public List<Requisition> searchApprovedRequisitions(String filterBy, List<UUID> desiredUuids) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Requisition> criteriaQuery = builder.createQuery(Requisition.class);

    Root<Requisition> root = criteriaQuery.from(Requisition.class);

    Path<UUID> facility = root.get(FACILITY_ID);
    Path<UUID> program = root.get(PROGRAM_ID);

    Predicate predicate =
        setFiltering(filterBy, builder, root, facility, program, desiredUuids);

    criteriaQuery = criteriaQuery.where(predicate);

    return entityManager.createQuery(criteriaQuery).getResultList();
  }

  /**
   * Get last regular requisition for the given facility and program.
   *
   * @param facility UUID of facility.
   * @param program  UUID of program.
   * @return last regular requisition for the given facility and program. {@code null} if not found.
   * @throws IllegalArgumentException if any of arguments is {@code null}.
   */
  @Override
  public Requisition getLastRegularRequisition(UUID facility, UUID program) {
    if (null == facility || null == program) {
      throw new IllegalArgumentException("facility and program must be provided");
    }

    CriteriaBuilder builder = entityManager.getCriteriaBuilder();

    CriteriaQuery<Requisition> query = builder.createQuery(Requisition.class);

    Root<Requisition> root = query.from(Requisition.class);

    Predicate predicate = builder.conjunction();
    predicate = builder.and(predicate, builder.equal(root.get(EMERGENCY), false));
    predicate = builder.and(predicate, builder.equal(root.get(FACILITY_ID), facility));
    predicate = builder.and(predicate, builder.equal(root.get(PROGRAM_ID), program));

    query.where(predicate);
    query.orderBy(builder.desc(root.get(CREATED_DATE)));

    List<Requisition> list = entityManager.createQuery(query).setMaxResults(1).getResultList();
    return null == list || list.isEmpty() ? null : list.get(0);
  }

  private Predicate setFiltering(String filterBy, CriteriaBuilder builder, Root<Requisition> root,
                                 Path<UUID> facility, Path<UUID> program, List<UUID> desiredUuids) {

    //Add first important filter
    Predicate predicate = builder.equal(root.get(STATUS), RequisitionStatus.APPROVED);

    if (filterBy != null && !filterBy.isEmpty()) {
      //Add second important filter
      Predicate predicateFilterBy = builder.disjunction();

      if (!desiredUuids.isEmpty()) {
        predicateFilterBy = builder.or(predicateFilterBy, facility.in(desiredUuids));
        predicateFilterBy = builder.or(predicateFilterBy, program.in(desiredUuids));
      }
      //Connector filters
      predicate = builder.and(predicate, predicateFilterBy);
    }

    return predicate;
  }

  private Predicate filterByStatuses(CriteriaBuilder builder, Predicate predicate,
                                     Set<RequisitionStatus> requisitionStatuses,
                                     Root<Requisition> root) {

    Predicate predicateToUse = predicate;

    if (requisitionStatuses != null && !requisitionStatuses.isEmpty()) {
      Predicate statusPredicate = builder.disjunction();
      for (RequisitionStatus status : requisitionStatuses) {
        statusPredicate = builder.or(statusPredicate,
            builder.equal(root.get(STATUS), status));
      }
      predicateToUse = builder.and(predicate, statusPredicate);
    }

    return predicateToUse;
  }
}