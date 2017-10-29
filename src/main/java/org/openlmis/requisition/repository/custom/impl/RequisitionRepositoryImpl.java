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

import org.apache.commons.lang3.tuple.Pair;
import org.hibernate.SQLQuery;
import org.hibernate.type.BooleanType;
import org.hibernate.type.PostgresUUIDType;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionPermissionString;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.custom.RequisitionRepositoryCustom;
import org.openlmis.requisition.utils.DateHelper;
import org.openlmis.requisition.utils.Pagination;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@SuppressWarnings({"PMD.CyclomaticComplexity", "PMD.TooManyMethods"})
public class RequisitionRepositoryImpl implements RequisitionRepositoryCustom {

  private static final String SEARCH_APPROVED_SQL = "SELECT"
      + " r.id AS req_id, r.emergency AS req_emergency,"
      + " r.facilityid AS facility_id, r.programid AS program_id, r.processingperiodid as period_id"
      + " FROM requisition.requisitions r"
      + " WHERE r.status = 'APPROVED'";

  private static final String FACILITY_ID = "facilityId";
  private static final String PROGRAM_ID = "programId";
  private static final String EMERGENCY = "emergency";
  private static final String STATUS = "status";
  private static final String CREATED_DATE = "createdDate";
  private static final String PROCESSING_PERIOD_ID = "processingPeriodId";
  private static final String SUPERVISORY_NODE_ID = "supervisoryNodeId";

  @PersistenceContext
  private EntityManager entityManager;

  @Autowired
  private DateHelper dateHelper;

  /**
   * Method returns all Requisitions with matched parameters. User permission strings must not be
   * empty.
   *
   * @param facilityId            Facility of searched Requisitions.
   * @param programId             Program of searched Requisitions.
   * @param initiatedDateFrom     After what date should searched Requisition be created.
   * @param initiatedDateTo       Before what date should searched Requisition be created.
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
      UUID processingPeriodId,
      UUID supervisoryNodeId,
      Set<RequisitionStatus> requisitionStatuses,
      Boolean emergency,
      List<String> userPermissionStrings,
      Pageable pageable) {

    CriteriaBuilder builder = entityManager.getCriteriaBuilder();

    CriteriaQuery<Requisition> requisitionQuery = builder.createQuery(Requisition.class);
    requisitionQuery = prepareQuery(requisitionQuery, facilityId, programId, initiatedDateFrom,
        initiatedDateTo, processingPeriodId, supervisoryNodeId, requisitionStatuses, emergency,
        userPermissionStrings, false, pageable);

    CriteriaQuery<Long> countQuery = builder.createQuery(Long.class);
    countQuery = prepareQuery(countQuery, facilityId, programId, initiatedDateFrom,
        initiatedDateTo, processingPeriodId, supervisoryNodeId, requisitionStatuses, emergency,
        userPermissionStrings, true, pageable);

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
    List<Requisition> results = entityManager.createQuery(query).getResultList();

    return results;
  }

  /**
   * Get approved requisitions matching all of provided parameters.
   *
   * @param filterBy    Field used to filter: programName, facilityCode, facilityName or all.
   * @param facilityIds Desired facility UUID list.
   * @param programIds  Desired program UUID list.
   * @return List of requisitions with required fields for convert.
   */
  @Override
  public List<Requisition> searchApprovedRequisitions(String filterBy,
                                                      Collection<UUID> facilityIds,
                                                      Collection<UUID> programIds) {
    Query query = createQuery(filterBy, facilityIds, programIds);
    addScalars(query);

    // hibernate always returns a list of array of objects
    @SuppressWarnings("unchecked")
    List<Object[]> list = Collections.checkedList(query.getResultList(), Object[].class);

    return list.stream().map(this::toRequisition).collect(Collectors.toList());
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

    CriteriaBuilder builder = entityManager.getCriteriaBuilder();

    CriteriaQuery<Requisition> query = builder.createQuery(Requisition.class);
    query = prepareApprovableQuery(builder, query, programNodePairs, false);

    CriteriaQuery<Long> countQuery = builder.createQuery(Long.class);
    countQuery = prepareApprovableQuery(builder, countQuery, programNodePairs, true);

    Long count = entityManager.createQuery(countQuery).getSingleResult();

    Pair<Integer, Integer> maxAndFirst = PageableUtil.querysMaxAndFirstResult(pageable);
    List<Requisition> requisitions = entityManager.createQuery(query)
        .setMaxResults(maxAndFirst.getLeft())
        .setFirstResult(maxAndFirst.getRight())
        .getResultList();

    return Pagination.getPage(requisitions, pageable, count);
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

    ZonedDateTime from = null;
    ZonedDateTime to = null;
    if (initiatedDateFrom != null) {
      from = initiatedDateFrom.atStartOfDay(dateHelper.getZone());
    }
    if (initiatedDateTo != null) {
      to = ZonedDateTime.of(initiatedDateTo, LocalTime.MAX, dateHelper.getZone());
    }

    if (from != null && to != null) {
      predicate = builder.and(predicate, builder.between(root.get(CREATED_DATE),
          from, to));
    } else if (from != null) {
      predicate = builder.and(predicate, builder.greaterThanOrEqualTo(
          root.get(CREATED_DATE), from));
    } else if (to != null) {
      predicate = builder.and(predicate, builder.lessThanOrEqualTo(
          root.get(CREATED_DATE), to));
    }

    Join<Requisition, RequisitionPermissionString> permissionStringJoin = root
        .join("permissionStrings");
    Expression<String> permissionStringExp = permissionStringJoin.get("permissionString");
    Predicate permissionStringPredicate = permissionStringExp.in(userPermissionStrings);
    predicate = builder.and(predicate, permissionStringPredicate);

    query.where(predicate);

    query.distinct(true);

    if (!count && pageable != null && pageable.getSort() != null) {
      query = addSortProperties(query, root, pageable);
    }

    return query;
  }

  private <T> CriteriaQuery<T> prepareApprovableQuery(CriteriaBuilder builder,
      CriteriaQuery<T> query, Set<Pair> programNodePairs, boolean isCountQuery) {

    Root<Requisition> root = query.from(Requisition.class);

    if (isCountQuery) {
      CriteriaQuery<Long> countQuery = (CriteriaQuery<Long>) query;
      query = (CriteriaQuery<T>) countQuery.select(builder.count(root));
    }

    List<Predicate> combinedPredicates = new ArrayList<>();
    for (Pair pair : programNodePairs) {
      Predicate programPredicate = builder.equal(root.get(PROGRAM_ID), pair.getLeft());
      Predicate nodePredicate = builder.equal(root.get(SUPERVISORY_NODE_ID), pair.getRight());
      Predicate combinedPredicate = builder.and(programPredicate, nodePredicate);
      combinedPredicates.add(combinedPredicate);
    }
    Predicate pairPredicate = builder.or(combinedPredicates.toArray(
        new Predicate[combinedPredicates.size()]));

    Predicate statusPredicate = builder.or(
        builder.equal(root.get(STATUS), RequisitionStatus.AUTHORIZED),
        builder.equal(root.get(STATUS), RequisitionStatus.IN_APPROVAL));

    Predicate predicate = builder.and(pairPredicate, statusPredicate);

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

  private <T> CriteriaQuery<T> addSortProperties(CriteriaQuery<T> query,
                                                 Root root, Pageable pageable) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    List<Order> orders = new ArrayList<>();
    Iterator<Sort.Order> iterator = pageable.getSort().iterator();

    Sort.Order order;
    while (iterator.hasNext()) {
      order = iterator.next();
      if (order.isAscending()) {
        orders.add(builder.asc(root.get(order.getProperty())));
      } else {
        orders.add(builder.desc(root.get(order.getProperty())));
      }
    }
    return query.orderBy(orders);
  }

  private Query createQuery(String filterBy, Collection<UUID> facilityIds,
                            Collection<UUID> programIds) {
    StringBuilder builder = new StringBuilder(SEARCH_APPROVED_SQL);

    if (!StringUtils.isEmpty(filterBy)) {
      String facilities = null;
      String programs = null;

      if (!CollectionUtils.isEmpty(facilityIds)) {
        facilities = String.format("r.facilityid in %s", mergeIds(facilityIds));
      }

      if (!CollectionUtils.isEmpty(programIds)) {
        programs = String.format("r.programid in %s", mergeIds(programIds));
      }

      if (facilities != null && programs != null) {
        builder.append(String.format(" AND (%s OR %s)", facilities, programs));
      } else if (facilities != null) {
        builder.append(String.format(" AND %s", facilities));
      } else if (programs != null) {
        builder.append(String.format(" AND %s", programs));
      }
    }

    return entityManager.createNativeQuery(builder.toString());
  }

  private void addScalars(Query query) {
    SQLQuery sql = query.unwrap(SQLQuery.class);
    sql.addScalar("req_id", PostgresUUIDType.INSTANCE);
    sql.addScalar("req_emergency", BooleanType.INSTANCE);
    sql.addScalar("facility_id", PostgresUUIDType.INSTANCE);
    sql.addScalar("program_id", PostgresUUIDType.INSTANCE);
    sql.addScalar("period_id", PostgresUUIDType.INSTANCE);
  }

  private String mergeIds(Collection<UUID> ids) {
    StringBuilder idsBuilder = new StringBuilder("(");
    String prefix = "";

    for (UUID facilityId : ids) {
      idsBuilder.append(prefix);
      idsBuilder.append(facilityId);
      prefix = ",";
    }
    idsBuilder.append(')');

    return idsBuilder.toString();
  }

  private Requisition toRequisition(Object[] values) {

    Requisition requisition = new Requisition();

    requisition.setId((UUID) values[0]);
    requisition.setEmergency((Boolean) values[1]);
    requisition.setFacilityId((UUID) values[2]);
    requisition.setProgramId((UUID) values[3]);
    requisition.setProcessingPeriodId((UUID) values[4]);

    return requisition;
  }
}