/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org.
 */

package org.openlmis.requisition.repository.custom.impl;

import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;

import com.google.common.base.Joiner;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.persistence.EntityGraph;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;
import org.apache.commons.lang3.tuple.Pair;
import org.hibernate.SQLQuery;
import org.hibernate.annotations.QueryHints;
import org.hibernate.type.BooleanType;
import org.hibernate.type.LongType;
import org.hibernate.type.PostgresUUIDType;
import org.hibernate.type.ZonedDateTimeType;
import org.openlmis.requisition.domain.BaseEntity;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionPermissionString;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.StatusChange;
import org.openlmis.requisition.repository.StatusChangeRepository;
import org.openlmis.requisition.repository.custom.RequisitionRepositoryCustom;
import org.openlmis.requisition.utils.DateHelper;
import org.openlmis.requisition.utils.Pagination;
import org.slf4j.ext.XLogger;
import org.slf4j.ext.XLoggerFactory;
import org.slf4j.profiler.Profiler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

@SuppressWarnings({"PMD.CyclomaticComplexity", "PMD.TooManyMethods"})
public class RequisitionRepositoryImpl implements RequisitionRepositoryCustom {

  private static final XLogger XLOGGER = XLoggerFactory.getXLogger(RequisitionRepositoryImpl.class);

  private static final String ORDER_BY = " ORDER BY ";

  private static final String FROM = " FROM requisition.requisitions r"
      + " INNER JOIN requisition.status_changes s ON r.id = s.requisitionid"
      + " WHERE r.status = 'APPROVED'"
      + " AND s.status = 'APPROVED'";

  private static final String SEARCH_APPROVED_SQL = "SELECT DISTINCT"
      + " r.id AS req_id, r.emergency AS req_emergency,"
      + " r.facilityid AS facility_id, r.programid AS program_id,"
      + " r.processingperiodid as period_id, r.supervisorynodeid as node_id,"
      + " s.createdDate as approved_date"
      + FROM;

  private static final String SELECT_COUNT_APPROVED_SQL = "SELECT DISTINCT COUNT(*)"
      + FROM;

  private static final String FACILITY_ID = "facilityId";
  private static final String PROGRAM_ID = "programId";
  private static final String EMERGENCY = "emergency";
  private static final String STATUS = "status";
  private static final String CREATED_DATE = "createdDate";
  private static final String PROCESSING_PERIOD_ID = "processingPeriodId";
  private static final String SUPERVISORY_NODE_ID = "supervisoryNodeId";
  private static final String AUTHORIZED_DATE = "authorizedDate";
  private static final String MODIFIED_DATE = "modifiedDate";
  private static final String WITH_PROGRAM_IDS =
      "r.programId IN :programIds";
  private static final String WITH_SUPERVISORY_NODE_IDS =
      "r.supervisoryNodeId IN :supervisoryNodeIds";

  @PersistenceContext
  private EntityManager entityManager;

  @Autowired
  private DateHelper dateHelper;

  @Autowired
  private StatusChangeRepository statusChangeRepository;

  /**
   * Method returns all Requisitions with matched parameters. User permission strings must not be
   * empty.
   *
   * @param facilityId            Facility of searched Requisitions.
   * @param programId             Program of searched Requisitions.
   * @param initiatedDateFrom     After what date should searched Requisition be created.
   * @param initiatedDateTo       Before what date should searched Requisition be created.
   * @param modifiedDateFrom      After what date should searched Requisition be modified.
   * @param modifiedDateTo        Before what date should searched Requisition be modified.
   * @param processingPeriodId    ProcessingPeriod of searched Requisitions.
   * @param supervisoryNodeId     SupervisoryNode of searched Requisitions.
   * @param requisitionStatuses   Statuses of searched Requisitions.
   * @param emergency             Requisitions with emergency status.
   * @param userPermissionStrings Permission strings of current user.
   * @return List of Requisitions with matched parameters.
   */
  @Override
  public Page<Requisition> searchRequisitions(UUID facilityId,
      UUID programId,
      LocalDate initiatedDateFrom,
      LocalDate initiatedDateTo,
      ZonedDateTime modifiedDateFrom,
      ZonedDateTime modifiedDateTo,
      UUID processingPeriodId,
      UUID supervisoryNodeId,
      Set<RequisitionStatus> requisitionStatuses,
      Boolean emergency,
      List<String> userPermissionStrings,
      Pageable pageable) {

    CriteriaBuilder builder = entityManager.getCriteriaBuilder();

    CriteriaQuery<Requisition> requisitionQuery = builder.createQuery(Requisition.class);
    requisitionQuery = prepareQuery(requisitionQuery, facilityId, programId, initiatedDateFrom,
        initiatedDateTo, modifiedDateFrom, modifiedDateTo, processingPeriodId, supervisoryNodeId,
        requisitionStatuses, emergency, userPermissionStrings, false, pageable);

    CriteriaQuery<Long> countQuery = builder.createQuery(Long.class);
    countQuery = prepareQuery(countQuery, facilityId, programId, initiatedDateFrom,
        initiatedDateTo, modifiedDateFrom, modifiedDateTo, processingPeriodId, supervisoryNodeId,
        requisitionStatuses, emergency, userPermissionStrings, true, pageable);

    Long count = entityManager.createQuery(countQuery).getSingleResult();

    Pair<Integer, Integer> maxAndFirst = PageableUtil.querysMaxAndFirstResult(pageable);
    List<Requisition> requisitions = entityManager.createQuery(requisitionQuery)
        .setMaxResults(maxAndFirst.getLeft())
        .setFirstResult(maxAndFirst.getRight())
        .getResultList();

    return Pagination.getPage(requisitions, pageable, count);
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
  public List<Requisition> searchRequisitions(UUID processingPeriod, UUID facility,
      UUID program, Boolean emergency) {
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
   * Empty list is returned if:
   * - facilityIds collection is empty and filterBy is facilityName or facilityCode,
   * - programIds collection is empty and filterBy is programName,
   * - both facilityIds and programIds collections are empty and filterBy is all.
   *
   * @param facilityId Desired facility UUID list.
   * @param programIds  Desired set of program UUIDs.
   * @param supervisoryNodeIds Desired set of supervisory node UUIDs.
   * @return List of requisitions with required fields for convert.
   */
  @Override
  public Page<Requisition> searchApprovedRequisitions(UUID facilityId, Set<UUID> programIds,
      Set<UUID> supervisoryNodeIds, Pageable pageable) {
    XLOGGER.entry(facilityId, programIds, supervisoryNodeIds, pageable);

    Query countQuery = createQuery(facilityId, programIds, supervisoryNodeIds,true, pageable);
    Long count = (Long) countQuery.getSingleResult();

    if (count == 0) {
      return Pagination.getPage(Collections.emptyList(), pageable, 0);
    }

    Query searchQuery = createQuery(facilityId, programIds, supervisoryNodeIds, false, pageable);
    addScalars(searchQuery);

    // hibernate always returns a list of array of objects
    @SuppressWarnings("unchecked")
    List<Object[]> list = Collections.checkedList(searchQuery
            .setFirstResult(pageable.getOffset())
            .setMaxResults(pageable.getPageSize())
            .getResultList(),
            Object[].class);

    List<Requisition> requisitions = list.stream().map(this::toRequisition)
        .collect(Collectors.toList());

    XLOGGER.exit(requisitions);
    return Pagination.getPage(requisitions, pageable, count);
  }

  /**
   * Get all requisitions that match any of the program/supervisoryNode pairs, that can be
   * approved (AUTHORIZED, IN_APPROVAL). Pairs must not be null.
   *
   * @param programNodePairs program / supervisoryNode pairs
   * @return matching requisitions
   */
  @Override
  public Page<Requisition> searchApprovableRequisitionsByProgramSupervisoryNodePairs(
      Set<Pair> programNodePairs, Pageable pageable) {
    XLOGGER.entry(programNodePairs, pageable);

    Profiler profiler = new Profiler("SEARCH_APPROBABLE_REQ_BY_PROGRAM_SUP_NODE_PAIRS");
    profiler.setLogger(XLOGGER);

    profiler.start("CREATE_BUILDER");
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();

    profiler.start("PREPARE_COUNT_QUERY");
    CriteriaQuery<Long> countQuery = builder.createQuery(Long.class);
    countQuery = prepareApprovableQuery(builder, countQuery, programNodePairs, true, pageable);

    profiler.start("EXECUTE_COUNT_QUERY");
    Long count = entityManager.createQuery(countQuery).getSingleResult();

    if (count == 0) {
      profiler.start("CREATE_RESULT_PAGE");
      Page<Requisition> page = Pagination.getPage(Collections.emptyList());

      XLOGGER.exit(page);
      profiler.stop().log();

      return page;
    }

    profiler.start("CREATE_ENTITY_GRAPH");
    EntityGraph graph = entityManager.createEntityGraph(Requisition.class);
    graph.addSubgraph(Requisition.STATUS_CHANGES);

    profiler.start("GET_MAX_AND_FIRST");
    final Pair<Integer, Integer> maxAndFirst = PageableUtil.querysMaxAndFirstResult(pageable);

    profiler.start("PREPARE_MAIN_QUERY");
    CriteriaQuery<Requisition> query = builder.createQuery(Requisition.class);
    query = prepareApprovableQuery(builder, query, programNodePairs, false, pageable);

    profiler.start("EXECUTE_MAIN_QUERY");
    List<Requisition> requisitions = entityManager.createQuery(query)
        .setHint(QueryHints.LOADGRAPH, graph)
        .setMaxResults(maxAndFirst.getLeft())
        .setFirstResult(maxAndFirst.getRight())
        .getResultList();

    profiler.start("GET_REQUISITIONS_IDS");
    Set<UUID> requisitionIds = requisitions
        .stream()
        .map(BaseEntity::getId)
        .collect(Collectors.toSet());

    profiler.start("GET_STATUS_CHANGES_BY_REQ_IDS");
    Map<UUID, List<StatusChange>> allStatusChanges = statusChangeRepository
        .findByRequisitionIdIn(requisitionIds)
        .stream()
        .collect(Collectors.groupingBy(status -> status.getRequisition().getId()));

    profiler.start("MATCH_REQ_WITH_STATUS_CHANGES");
    requisitions
        .forEach(requisition -> {
          List<StatusChange> statusChanges = allStatusChanges.get(requisition.getId());
          requisition.setStatusChanges(statusChanges);
        });

    profiler.start("CREATE_RESULT_PAGE");
    Page<Requisition> page = Pagination.getPage(requisitions, pageable, count);

    XLOGGER.exit(page);
    profiler.stop().log();

    return page;
  }

  /**
   * Get requisition with the given id.
   *
   * @param requisitionId UUID of requisition.
   * @return requisition with given id. {@code null} if not found.
   * @throws IllegalArgumentException if any of arguments is {@code null}.
   */
  public Requisition findOne(UUID requisitionId) {
    if (requisitionId == null) {
      throw new IllegalArgumentException("Requisition's id must be provided");
    }
    return entityManager.find(Requisition.class, requisitionId);
  }

  private <T> CriteriaQuery<T> prepareQuery(CriteriaQuery<T> query,
      UUID facilityId,
      UUID programId,
      LocalDate initiatedDateFrom,
      LocalDate initiatedDateTo,
      ZonedDateTime modifiedDateFrom,
      ZonedDateTime modifiedDateTo,
      UUID processingPeriodId,
      UUID supervisoryNodeId,
      Set<RequisitionStatus> requisitionStatuses,
      Boolean emergency,
      List<String> userPermissionStrings,
      boolean count,
      Pageable pageable) {

    CriteriaBuilder builder = entityManager.getCriteriaBuilder();

    Root<Requisition> root = query.from(Requisition.class);

    if (count) {
      CriteriaQuery<Long> countQuery = (CriteriaQuery<Long>) query;
      query = (CriteriaQuery<T>) countQuery.select(builder.count(root));
    } else {
      query.orderBy(builder.asc(root.get(CREATED_DATE)));
    }

    Predicate predicate = builder.conjunction();
    if (facilityId != null) {
      predicate = builder.and(predicate, builder.equal(root.get(FACILITY_ID), facilityId));
    }
    if (programId != null) {
      predicate = builder.and(predicate, builder.equal(root.get(PROGRAM_ID), programId));
    }
    if (processingPeriodId != null) {
      predicate = builder.and(predicate,
          builder.equal(root.get(PROCESSING_PERIOD_ID), processingPeriodId));
    }
    if (supervisoryNodeId != null) {
      predicate = builder.and(predicate,
          builder.equal(root.get(SUPERVISORY_NODE_ID), supervisoryNodeId));
    }
    predicate = filterByStatuses(builder, predicate, requisitionStatuses, root);
    if (null != emergency) {
      predicate = builder.and(predicate,
          builder.equal(root.get(EMERGENCY), emergency));
    }

    ZonedDateTime fromInitiatedDate = setStartDateParam(initiatedDateFrom);
    ZonedDateTime toInitiatedDate = setEndDateParam(initiatedDateTo);
    predicate = setDateRangeParameters(predicate, builder, root, fromInitiatedDate,
        toInitiatedDate, CREATED_DATE);

    ZonedDateTime fromModifiedDate = setStartDateParam(modifiedDateFrom != null
        ? modifiedDateFrom.toLocalDate() : null);
    ZonedDateTime toModifiedDate = setEndDateParam(modifiedDateTo != null
        ? modifiedDateTo.toLocalDate() : null);
    predicate = setDateRangeParameters(predicate, builder, root, fromModifiedDate,
        toModifiedDate, MODIFIED_DATE);

    Join<Requisition, RequisitionPermissionString> permissionStringJoin = root
        .join("permissionStrings");
    Expression<String> permissionStringExp = permissionStringJoin.get("permissionString");
    Predicate permissionStringPredicate = permissionStringExp.in(userPermissionStrings);
    predicate = builder.and(predicate, permissionStringPredicate);

    query.where(predicate);

    query.distinct(true);

    if (!count && pageable != null && pageable.getSort() != null) {
      query = addSortProperties(builder, query, root, pageable);
    }

    return query;
  }

  private <T> CriteriaQuery<T> prepareApprovableQuery(CriteriaBuilder builder,
      CriteriaQuery<T> query, Set<Pair> programNodePairs, boolean isCountQuery, Pageable pageable) {

    Root<Requisition> root = query.from(Requisition.class);

    if (isCountQuery) {
      CriteriaQuery<Long> countQuery = (CriteriaQuery<Long>) query;
      query = (CriteriaQuery<T>) countQuery.select(builder.count(root));
    } else {
      query.distinct(true);
    }

    List<Predicate> combinedPredicates = new ArrayList<>();
    for (Pair pair : programNodePairs) {
      Predicate programPredicate = builder.equal(root.get(PROGRAM_ID), pair.getLeft());
      Predicate nodePredicate = builder.equal(root.get(SUPERVISORY_NODE_ID), pair.getRight());
      Predicate combinedPredicate = builder.and(programPredicate, nodePredicate);
      combinedPredicates.add(combinedPredicate);
    }
    Predicate pairPredicate = builder.or(combinedPredicates.toArray(new Predicate[0]));

    Predicate statusPredicate = builder.or(
        builder.equal(root.get(STATUS), RequisitionStatus.AUTHORIZED),
        builder.equal(root.get(STATUS), RequisitionStatus.IN_APPROVAL));

    Predicate predicate = builder.and(pairPredicate, statusPredicate);

    if (!isCountQuery) {
      Subquery<ZonedDateTime> subquery = query.subquery(ZonedDateTime.class);
      Root<StatusChange> subRoot = subquery.from(StatusChange.class);

      subquery.select(builder.greatest(subRoot.<ZonedDateTime>get(CREATED_DATE)));
      subquery.where(builder.and(
          builder.equal(subRoot.get(STATUS), RequisitionStatus.AUTHORIZED),
          builder.equal(subRoot.get("requisition"), root)));

      Join<Requisition, StatusChange> statusChanges = root.join(Requisition.STATUS_CHANGES);
      predicate = builder.and(predicate,
          builder.equal(statusChanges.get(STATUS), RequisitionStatus.AUTHORIZED),
          statusChanges.get(CREATED_DATE).in(subquery));
    }

    if (!isCountQuery && pageable != null && pageable.getSort() != null) {
      query = addSortProperties(builder, query, root, pageable);
    }

    return query.where(predicate);
  }

  private Predicate filterByStatuses(CriteriaBuilder builder, Predicate predicate,
      Set<RequisitionStatus> requisitionStatuses,
      Root<Requisition> root) {

    Predicate predicateToUse = predicate;

    if (requisitionStatuses != null && !requisitionStatuses.isEmpty()) {
      Predicate statusPredicate = builder.disjunction();
      for (RequisitionStatus status : requisitionStatuses) {
        statusPredicate = builder.or(statusPredicate, builder.equal(root.get(STATUS), status));
      }
      predicateToUse = builder.and(predicate, statusPredicate);
    }

    return predicateToUse;
  }

  private <T> CriteriaQuery<T> addSortProperties(CriteriaBuilder builder,
      CriteriaQuery<T> query, Root<Requisition> root, Pageable pageable) {
    List<Order> orders = new ArrayList<>();
    Iterator<Sort.Order> iterator = pageable.getSort().iterator();
    Sort.Order order;

    while (iterator.hasNext()) {
      order = iterator.next();
      String property = order.getProperty();

      Path path;

      if (AUTHORIZED_DATE.equals(property)) {
        path = root
            .getJoins()
            .stream()
            .filter(item -> Requisition.STATUS_CHANGES.equals(item.getAttribute().getName()))
            .findFirst()
            .orElseThrow(() -> new IllegalStateException("Can't find statusChanges join"))
            .get(CREATED_DATE);
      } else {
        path = root.get(property);
      }

      if (order.isAscending()) {
        orders.add(builder.asc(path));
      } else {
        orders.add(builder.desc(path));
      }
    }

    return query.orderBy(orders);
  }

  private Query createQuery(UUID facilityId, Set<UUID> programIds, Set<UUID> supervisoryNodeIds,
      Boolean count, Pageable pageable) {
    StringBuilder builder = new StringBuilder(
        count ? SELECT_COUNT_APPROVED_SQL : SEARCH_APPROVED_SQL);

    if (null != facilityId) {
      builder.append(String.format(" AND r.facilityid = '%s'", facilityId));
    }
    if (isNotEmpty(programIds)) {
      builder.append(String.format(" AND " + WITH_PROGRAM_IDS));
    }
    if (isNotEmpty(supervisoryNodeIds)) {
      builder.append(String.format(" AND " + WITH_SUPERVISORY_NODE_IDS));
    }

    if (!count && pageable.getSort() != null) {
      builder.append(ORDER_BY);
      builder.append(getOrderPredicate(pageable));
    }

    Query query = entityManager.createNativeQuery(builder.toString());

    if (isNotEmpty(programIds)) {
      query.setParameter("programIds", programIds);
    }

    if (isNotEmpty(supervisoryNodeIds)) {
      query.setParameter("supervisoryNodeIds", supervisoryNodeIds);
    }

    if (count) {
      addScalarsForCount(query);
    }

    return query;
  }

  private void addScalars(Query query) {
    SQLQuery sql = query.unwrap(SQLQuery.class);
    sql.addScalar("req_id", PostgresUUIDType.INSTANCE);
    sql.addScalar("req_emergency", BooleanType.INSTANCE);
    sql.addScalar("facility_id", PostgresUUIDType.INSTANCE);
    sql.addScalar("program_id", PostgresUUIDType.INSTANCE);
    sql.addScalar("period_id", PostgresUUIDType.INSTANCE);
    sql.addScalar("node_id", PostgresUUIDType.INSTANCE);
    sql.addScalar("approved_date", ZonedDateTimeType.INSTANCE);
  }

  private void addScalarsForCount(Query query) {
    SQLQuery sql = query.unwrap(SQLQuery.class);
    sql.addScalar("count", LongType.INSTANCE);
  }

  private Requisition toRequisition(Object[] values) {

    Requisition requisition = new Requisition();

    requisition.setId((UUID) values[0]);
    requisition.setEmergency((Boolean) values[1]);
    requisition.setFacilityId((UUID) values[2]);
    requisition.setProgramId((UUID) values[3]);
    requisition.setProcessingPeriodId((UUID) values[4]);
    requisition.setSupervisoryNodeId((UUID) values[5]);

    StatusChange statusChange = new StatusChange();
    statusChange.setCreatedDate((ZonedDateTime) values[6]);
    statusChange.setStatus(RequisitionStatus.APPROVED);
    requisition.setStatusChanges(Collections.singletonList(statusChange));

    return requisition;
  }

  private String getOrderPredicate(Pageable pageable) {
    List<String> orderPredicate = new ArrayList<>();
    List<String> sql = new ArrayList<>();
    Iterator<Sort.Order> iterator = pageable.getSort().iterator();
    Sort.Order order;
    Sort.Direction sortDirection = Sort.Direction.ASC;

    while (iterator.hasNext()) {
      order = iterator.next();
      orderPredicate.add("r.".concat(order.getProperty()));
      sortDirection = order.getDirection();
    }

    sql.add(Joiner.on(",").join(orderPredicate));
    sql.add(sortDirection.name());

    return Joiner.on(' ').join(sql);
  }

  private ZonedDateTime setStartDateParam(LocalDate dateFrom) {
    return dateFrom != null ? dateFrom.atStartOfDay(dateHelper.getZone()) : null;
  }

  private ZonedDateTime setEndDateParam(LocalDate dateTo) {
    return dateTo != null
        ? ZonedDateTime.of(dateTo, LocalTime.MAX, dateHelper.getZone()) : null;
  }

  private Predicate setDateRangeParameters(Predicate predicate, CriteriaBuilder builder,
      Root<Requisition> root, ZonedDateTime startDate, ZonedDateTime endDate, String date) {
    if (startDate != null && endDate != null) {
      predicate = builder.and(predicate, builder.between(root.get(date),
          startDate, endDate));
    } else if (startDate != null) {
      predicate = builder.and(predicate, builder.greaterThanOrEqualTo(
          root.get(date), startDate));
    } else if (endDate != null) {
      predicate = builder.and(predicate, builder.lessThanOrEqualTo(
          root.get(date), endDate));
    }
    return predicate;
  }
}
