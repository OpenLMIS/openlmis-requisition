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

import org.javers.core.Javers;
import org.javers.core.diff.Change;
import org.javers.core.diff.changetype.ValueChange;
import org.javers.repository.jql.JqlQuery;
import org.javers.repository.jql.QueryBuilder;
import org.joda.time.LocalDateTime;
import org.openlmis.JaVersDateProvider;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusLogEntry;
import org.openlmis.requisition.repository.custom.RequisitionRepositoryCustom;
import org.openlmis.util.Pagination;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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

  @Autowired
  private Javers javers;

  @PersistenceContext
  private EntityManager entityManager;


  /**
   * Method returns all Requisitions with matched parameters.
   *
   * @param facility            Facility of searched Requisitions.
   * @param program             Program of searched Requisitions.
   * @param initiatedDateFrom   After what date should searched Requisition be created.
   * @param initiatedDateTo     Before what date should searched Requisition be created.
   * @param processingPeriod    ProcessingPeriod of searched Requisitions.
   * @param supervisoryNode     SupervisoryNode of searched Requisitions.
   * @param requisitionStatuses Statuses of searched Requisitions.
   * @return List of Requisitions with matched parameters.
   */
  @Override
  public Page<Requisition> searchRequisitions(UUID facility, UUID program,
                                              ZonedDateTime initiatedDateFrom,
                                              ZonedDateTime initiatedDateTo,
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
    queryMain.orderBy(builder.asc(root.get(CREATED_DATE)));


    /*
    int pageNumber = Pagination.getPageNumber(pageable);
    int pageSize = Pagination.getPageSize(pageable);

    List<Requisition> results = entityManager.createQuery(queryMain)
                                .setFirstResult(pageNumber * pageSize)
                                .setMaxResults(pageSize)
                                .getResultList();
                                */

    List<Requisition> results = entityManager.createQuery(queryMain).getResultList();

    //Get requisitions initiated between the initiatedDateFrom and initiatedDateTo dates
    List<UUID> validIds = getSortedRequisitionIdValuesInitiatedBetweenDates(initiatedDateFrom,
            initiatedDateTo, true);
    //Remove requisitions which aren't included in the above list
    for (int i = 0; i < results.size(); i++) {
      UUID resultId = results.get(i).getId();
      if (!validIds.contains(resultId)) {
        results.remove(i);
        i--;
      }
    }

    addStatusChangesToRequisitions(results);

    //Having retrieved just paginated values we care about, determine
    //the total number of values in the system which meet our criteria.
    /*
    CriteriaQuery<Long> queryCount = builder.createQuery(Long.class);
    Root<Requisition> rootQueryCount = queryCount.from(Requisition.class);
    queryCount.select(builder.count(rootQueryCount));
    queryCount.where(predicate);
    Long count = entityManager.createQuery(queryCount).getSingleResult(); */


    return Pagination.getPage(results, pageable);
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
    List<Requisition> results = entityManager.createQuery(query).getResultList();

    addStatusChangesToRequisitions(results);
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

    addStatusChangesToRequisitions(results);
    return results;
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
    // TODO: OLMIS-1182 this needs to search Javers for initiated date
    query.orderBy(builder.desc(root.get(CREATED_DATE)));

    /*
    List<Requisition> requisitionList = entityManager.createQuery(query)
                                        .setMaxResults(1).getResultList();

    if (requisitionList == null || requisitionList.isEmpty()) {
      return null;
    }

    Requisition requisition = requisitionList.get(0); */

    List<Requisition> requisitionList = entityManager.createQuery(query).getResultList();
    if (requisitionList == null || requisitionList.isEmpty()) {
      return null;
    }

    //Retrieve requisitions in descending order of their status' change to INITIATED
    List<UUID> sortedIds = getSortedRequisitionIdValuesInitiatedBetweenDates(null, null, false);

    //Return the first requisition in sortedIds which appears in requisitionList
    UUID uuid;
    Requisition requisition;
    for (int i = 0; i < sortedIds.size(); i++) {
      uuid = sortedIds.get(i);
      for (int x = 0; x < requisitionList.size(); x++) {
        requisition = requisitionList.get(x);
        if (requisition.getId().equals(uuid)) {
          return addStatusChangesToRequisition(requisition);
        }
      }
    }

    return null;
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
    Requisition requisition = entityManager.find(Requisition.class, requisitionId);
    if (requisition != null) {
      addStatusChangesToRequisition(requisition);
    }
    return requisition;
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


  /**
   * <p> Returns the UUID values of requisitions which have had their status set to INITIATED
   * within a specified time range.
   * </p><p>
   * Note that it's possible for requisitions to be set to INITIATED, then to something else,
   * and then back to INITIATED. This might happen several times. Only the initial or the most
   * recent status change to INITIATED is examined by this method, depending on the value of
   * useInitialChange.
   * </p><p>
   * The UUIDs are returned in order of their associated changes. If useInitialChange is true, the
   * order is ascending. Otherwise it's descending.
   * </p><p>
   * The startDate and endDate arguments may be null, in which case they're effectively ignored.
   * </p>
   * @param startDate The DateTime before which requisitions set to INITIATED should be excluded.
   * @param endDate The DateTime after which requisitions set to INITIATED should be excluded.
   * @param useInitialChange If true, specifies that the DateTime of the first status-change to
   *                         INITIATED should be used. If false, specifies that the DateTime of the
   *                         most recent status change to INITIATED should be used.
   */
  private List<UUID> getSortedRequisitionIdValuesInitiatedBetweenDates(ZonedDateTime startDate,
                                                                       ZonedDateTime endDate,
                                                                       boolean useInitialChange) {
    //Get JaVers' audit log
    JqlQuery jqlQuery = QueryBuilder.byClass(Requisition.class).withChildValueObjects()
            .withNewObjectChanges().andProperty("status").build();

    List<Change> changes = javers.findChanges(jqlQuery);

    //Sort it...
    if (useInitialChange) {
      //... in ascending order...
      changes.sort((o1, o2) -> 1 * o1.getCommitMetadata().get().getCommitDate()
              .compareTo(o2.getCommitMetadata().get().getCommitDate()));
    } else {
      //...or descending order.
      changes.sort((o1, o2) -> -1 * o1.getCommitMetadata().get().getCommitDate()
              .compareTo(o2.getCommitMetadata().get().getCommitDate()));
    }

    String newValue;
    String idString;
    UUID requisitionId;
    ValueChange valueChange;
    LocalDateTime javersLocalDateTime;
    ZonedDateTime javersZonedDateTime;
    List<UUID> results = new ArrayList<UUID>();

    if (startDate == null) {
      startDate = JaVersDateProvider.getMinDateTime();
    }

    if (endDate == null) {
      endDate = JaVersDateProvider.getMaxDateTime();
    }


    for (int i = 0; i < changes.size(); i++) {
      valueChange = (ValueChange) changes.get(i);
      newValue = valueChange.getRight().toString().replace("value:", "");

      //...heeding only INITIATED ones
      if (!newValue.equals("INITIATED")) {
        continue;
      }

      //Get the commit's DateTime
      javersLocalDateTime = valueChange.getCommitMetadata().get().getCommitDate();
      javersZonedDateTime = JaVersDateProvider.getZonedDateTime(javersLocalDateTime);
      
      //Get the UUID of the requisition
      idString = valueChange.getAffectedGlobalId().value();
      idString = idString.substring((idString.lastIndexOf('/') + 1));
      requisitionId = UUID.fromString(idString);

      //Note the commit if appropriate
      if ( !results.contains(requisitionId)
              && (!javersZonedDateTime.isBefore(startDate)
              && !javersZonedDateTime.isAfter(endDate))) {
        results.add(requisitionId);
      }
    }

    return results;
  }


  private List<Requisition> addStatusChangesToRequisitions(List<Requisition> requisitions) {
    for (Requisition requisition : requisitions) {
      addStatusChangesToRequisition(requisition);
    }
    return requisitions;
  }

  private Requisition addStatusChangesToRequisition(Requisition requisition) {
    JqlQuery jqlQuery = QueryBuilder.byInstanceId(requisition.getId(), Requisition.class)
            .withChildValueObjects().withNewObjectChanges()
            .andProperty("status").build();

    List<Change> changes = javers.findChanges(jqlQuery);

    //Sort: most recent commits first
    changes.sort((o1, o2) -> -1 * o1.getCommitMetadata().get().getCommitDate()
            .compareTo(o2.getCommitMetadata().get().getCommitDate()));

    String newValue;
    String commitAuthorString;
    UUID commitAuthorUuid;
    LocalDateTime javersLocalDateTime;
    ZonedDateTime javersZonedDateTime;
    ValueChange valueChange;
    Map<String, StatusLogEntry> statusChanges = new HashMap<String, StatusLogEntry>();

    for (int i = 0; i < changes.size(); i++) {

      //Get the newValue
      valueChange = (ValueChange)changes.get(i);
      newValue = valueChange.getRight().toString().replace("value:", "");

      if (!statusChanges.containsKey(newValue)) {

        /* Get the commitAuthorString.
           Note that it isn't guaranteed to be a valid UUID because it may be something like
           "unauthenticated user."  */
        commitAuthorString = valueChange.getCommitMetadata().get().getAuthor();
        try {
          commitAuthorUuid = UUID.fromString(commitAuthorString);
        } catch (Exception ex) {
          commitAuthorUuid = null;
        }

        //Get a ZonedDateTime coresponding to the commit's LocalDateTime
        javersLocalDateTime = valueChange.getCommitMetadata().get().getCommitDate();
        javersZonedDateTime = JaVersDateProvider.getZonedDateTime(javersLocalDateTime);

        StatusLogEntry statusLogEntry = new StatusLogEntry(commitAuthorUuid, javersZonedDateTime);
        statusChanges.put(newValue, statusLogEntry);
      }
    }

    requisition.setStatusChanges(statusChanges);
    return requisition;
  }

}
