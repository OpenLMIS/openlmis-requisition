package org.openlmis.requisition.repository.custom.impl;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.repository.custom.RequisitionRepositoryCustom;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

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
  public List<Requisition> searchRequisitions(UUID facility, UUID program,
                                              LocalDateTime createdDateFrom,
                                              LocalDateTime createdDateTo,
                                              UUID processingPeriod,
                                              UUID supervisoryNode,
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

  /**
   * Get approved requisitions matching all of provided parameters.
   *
   * @param filterValue Value to be used to filter.
   * @param filterBy Field used to filter: "programName", "facilityCode", "facilityName" or "all".
   * @param sortBy Field used to sort: "programName", "facilityCode" or "facilityName".
   * @param descending Descending direction for sort.
   * @param pageNumber Page number to return.
   * @param pageSize Quantity for one page.
   *
   * @return List of requisitions.
   */
  @Override
  public List<Requisition> searchApprovedRequisitionsWithSortAndFilterAndPaging(
      String filterValue, String filterBy, String sortBy, Boolean descending,
      Integer pageNumber, Integer pageSize) {

    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Requisition> criteriaQuery = builder.createQuery(Requisition.class);

    Root<Requisition> root = criteriaQuery.from(Requisition.class);

    Path<FacilityDto> facility = root.get("facility");
    Path<String> facilityCode = facility.get("code");
    Path<String> facilityName = facility.get("name");

    Path<ProgramDto> program = root.get("program");
    Path<String> programName = program.get("name");

    Predicate predicate = setFiltering(filterValue, filterBy, builder, root, facilityCode,
        facilityName, programName);

    List<Order> orders = setSorting(builder, sortBy, descending, root,
        programName, facilityCode, facilityName);

    criteriaQuery = criteriaQuery.where(predicate).orderBy(orders);
    Query query = entityManager.createQuery(criteriaQuery);

    query = setPaging(query, pageNumber, pageSize);

    return query.getResultList();
  }

  private Predicate setFiltering(String filterValue, String filterBy, CriteriaBuilder builder,
                                 Root<Requisition> root, Path<String> facilityCode,
                                 Path<String> facilityName, Path<String> programName) {

    //Add second important filter
    filterValue = "%" + filterValue + "%";
    Predicate predicateFilterBy = builder.disjunction();
    if (filterBy.equals("programName") || filterBy.equals("all")) {
      predicateFilterBy =
          builder.or(predicateFilterBy, builder.like(programName, filterValue));
    }
    if (filterBy.equals("facilityCode") || filterBy.equals("all")) {
      predicateFilterBy =
          builder.or(predicateFilterBy, builder.like(facilityCode, filterValue));
    }
    if (filterBy.equals("facilityName") || filterBy.equals("all")) {
      predicateFilterBy =
          builder.or(predicateFilterBy, builder.like(facilityName, filterValue));
    }
    //Add first important filter
    Predicate predicate = builder.equal(root.get("status"), RequisitionStatus.APPROVED);
    //Connector filters
    predicate = builder.and(predicate, predicateFilterBy);
    return predicate;
  }

  private List<Order> setSorting(CriteriaBuilder builder, String sortBy, Boolean descending,
                                 Path root, Path programName, Path facilityCode,
                                 Path facilityName) {

    Path<ProcessingPeriodDto> rootProcesingPeriod = root.get("processingPeriod");
    Path<LocalDate> rootProcesingPeriodEndDate = rootProcesingPeriod.get("endDate");
    Path<ProcessingScheduleDto> rootProcesingPeriodProcessingSchedule =
        rootProcesingPeriod.get("processingSchedule");
    Path<LocalDateTime> rootProcesingPeriodProcesingScheduleModifiedDate =
        rootProcesingPeriodProcessingSchedule.get("modifiedDate");
    List<Order> orders = new ArrayList<>();
    Path setSortBy;
    switch (sortBy) {
      case "programName":
        setSortBy = programName;
        break;
      case "facilityCode":
        setSortBy = facilityCode;
        break;
      default:
        setSortBy = facilityName;
        break;
    }
    //Set first sorting
    if (descending == Boolean.TRUE) {
      orders.add(builder.desc(setSortBy));
    } else {
      orders.add(builder.asc(setSortBy));
    }
    //Set second sorting
    if (setSortBy.equals(programName)) {
      orders.add(builder.desc(rootProcesingPeriodEndDate));
    } else {
      orders.add(builder.desc(rootProcesingPeriodProcesingScheduleModifiedDate));
    }
    return orders;
  }

  private Query setPaging(Query query, Integer pageNumber, Integer pageSize) {
    query.setFirstResult((pageNumber - 1) * pageSize);
    query.setMaxResults(pageSize);
    return query;
  }
}
