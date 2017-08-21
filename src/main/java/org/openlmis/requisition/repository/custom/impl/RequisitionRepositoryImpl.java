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

import static java.util.stream.Collectors.toList;

import java.time.ZonedDateTime;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionPermissionString;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.custom.RequisitionRepositoryCustom;

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
   * Method returns all Requisitions with matched parameters. User permission strings must not be
   * empty.
   *
   * @param facility              Facility of searched Requisitions.
   * @param program               Program of searched Requisitions.
   * @param initiatedDateFrom     After what date should searched Requisition be created.
   * @param initiatedDateTo       Before what date should searched Requisition be created.
   * @param processingPeriod      ProcessingPeriod of searched Requisitions.
   * @param supervisoryNode       SupervisoryNode of searched Requisitions.
   * @param requisitionStatuses   Statuses of searched Requisitions.
   * @param emergency             Requisitions with emergency status.
   * @param userPermissionStrings Permission strings of current user.
   * @return List of Requisitions with matched parameters.
   */
  @Override
  public List<Requisition> searchRequisitions(UUID facility, UUID program,
      ZonedDateTime initiatedDateFrom,
      ZonedDateTime initiatedDateTo,
      UUID processingPeriod,
      UUID supervisoryNode,
      Set<RequisitionStatus> requisitionStatuses,
      Boolean emergency,
      List<String> userPermissionStrings) {

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

    if (initiatedDateFrom != null && initiatedDateTo != null) {
      predicate = builder.and(predicate, builder.between(root.get(CREATED_DATE),
          initiatedDateFrom, initiatedDateTo));
    } else if (initiatedDateFrom != null) {
      predicate = builder.and(predicate, builder.greaterThanOrEqualTo(
          root.get(CREATED_DATE), initiatedDateFrom));
    } else if (initiatedDateTo != null) {
      predicate = builder.and(predicate, builder.lessThanOrEqualTo(
          root.get(CREATED_DATE), initiatedDateTo));
    }

    Join<Requisition, RequisitionPermissionString> permissionStringJoin = root
        .join("permissionStrings");
    Expression<String> permissionStringExp = permissionStringJoin.get("permissionString");
    Predicate permissionStringPredicate = permissionStringExp.in(userPermissionStrings);
    predicate = builder.and(predicate, permissionStringPredicate);

    queryMain.where(predicate);
    queryMain.orderBy(builder.asc(root.get(CREATED_DATE)));

    List<Requisition> results = entityManager.createQuery(queryMain).getResultList();
    List<Requisition> distinctResults = results.stream().distinct().collect(toList());

    return distinctResults;
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

    Predicate predicate = setFiltering(filterBy, builder, root, facility, program, desiredUuids);

    criteriaQuery = criteriaQuery.where(predicate);
    List<Requisition> results = entityManager.createQuery(criteriaQuery).getResultList();

    return results;
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
        statusPredicate = builder.or(statusPredicate, builder.equal(root.get(STATUS), status));
      }
      predicateToUse = builder.and(predicate, statusPredicate);
    }

    return predicateToUse;
  }
}