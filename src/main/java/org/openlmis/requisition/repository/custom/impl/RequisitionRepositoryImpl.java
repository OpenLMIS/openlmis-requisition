package org.openlmis.requisition.repository.custom.impl;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.repository.custom.RequisitionRepositoryCustom;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.utils.RequisitionDtoComparator;
import org.springframework.beans.factory.annotation.Autowired;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

public class RequisitionRepositoryImpl implements RequisitionRepositoryCustom {

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private PeriodReferenceDataService processingPeriodReferenceDataService;


  @PersistenceContext
  private EntityManager entityManager;

  /**
   * Method returns all Requisitions with matched parameters.
   *
   * @param facility          facility of searched Requisitions.
   * @param program           program of searched Requisitions.
   * @param createdDateFrom   After what date should searched Requisition be created.
   * @param createdDateTo     Before what date should searched Requisition be created.
   * @param processingPeriod  processingPeriod of searched Requisitions.
   * @param supervisoryNode   supervisoryNode of searched Requisitions.
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
      predicate = builder.and(predicate, builder.equal(root.get("facilityId"), facility));
    }
    if (program != null) {
      predicate = builder.and(predicate, builder.equal(root.get("programId"), program));
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
              builder.equal(root.get("processingPeriodId"), processingPeriod));
    }
    if (supervisoryNode != null) {
      predicate = builder.and(predicate,
              builder.equal(root.get("supervisoryNodeId"), supervisoryNode));
    }
    if (requisitionStatus != null) {
      predicate = builder.and(predicate,
          builder.equal(root.get("status"), requisitionStatus));
    }

    query.where(predicate);
    return entityManager.createQuery(query).getResultList();
  }


  /**
   * Method returns all Requisitions with matched parameters.
   *
   * @param processingPeriod processingPeriod of searched Requisitions.
   * @return list of Requisitions with matched parameters.
   */
  public List<Requisition> searchByProcessingPeriod(UUID processingPeriod) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Requisition> query = builder.createQuery(Requisition.class);
    Root<Requisition> root = query.from(Requisition.class);
    Predicate predicate = builder.conjunction();
    if (processingPeriod != null) {
      predicate = builder.and(predicate,
            builder.equal(root.get("processingPeriodId"), processingPeriod));
    }
    query.where(predicate);
    return entityManager.createQuery(query).getResultList();
  }

  /**
   * Get approved requisitions matching all of provided parameters.
   *
   * @param filterValue Value to be used to filter.
   * @param filterBy    Field used to filter: "programName","facilityCode","facilityName" or "all".
   * @param sortBy      Field used to sort: "programName", "facilityCode" or "facilityName".
   * @param descending  Descending direction for sort.
   * @param pageNumber  Page number to return.
   * @param pageSize    Quantity for one page.
   * @return List of requisitions.
   */
  @Override
  public List<RequisitionDto> searchApprovedRequisitionsWithSortAndFilterAndPaging(
      String filterValue, String filterBy, String sortBy, Boolean descending,
      Integer pageNumber, Integer pageSize) {

    List<Requisition> filteredRequisitions = filterRequisitions(filterValue, filterBy);
    List<RequisitionDto> filteredRequisitionsDto =
        convertRequisitionListToRequisitionDtoList(filteredRequisitions);

    filteredRequisitionsDto.sort(new RequisitionDtoComparator(sortBy));
    if (!descending) {
      Collections.reverse(filteredRequisitionsDto);
    }

    int firstPageRecordListIndex = (pageNumber - 1) * pageSize;
    int lastPageRecordListIndex = (pageNumber * pageSize) - 1;

    if (firstPageRecordListIndex > filteredRequisitionsDto.size()) {
      return null;
    }
    if (lastPageRecordListIndex > filteredRequisitionsDto.size()) {
      lastPageRecordListIndex = filteredRequisitionsDto.size() - 1;
    }

    return filteredRequisitionsDto.subList(firstPageRecordListIndex, lastPageRecordListIndex);
  }

  private List<Requisition> filterRequisitions(String filterValue, String filterBy) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Requisition> criteriaQuery = builder.createQuery(Requisition.class);

    Root<Requisition> root = criteriaQuery.from(Requisition.class);

    Path<UUID> facility = root.get("facilityId");
    Path<UUID> program = root.get("programId");


    Predicate predicate =
        setFiltering(filterValue, filterBy, builder, root, facility, program);

    criteriaQuery = criteriaQuery.where(predicate);
    Query query = entityManager.createQuery(criteriaQuery);

    return query.getResultList();
  }

  private Predicate setFiltering(String filterValue, String filterBy, CriteriaBuilder builder,
                                 Root<Requisition> root, Path<UUID> facility, Path<UUID> program) {

    //Add second important filter
    List<UUID> desiredUuids = findDesiredUuids(filterValue, filterBy);
    Predicate predicateFilterBy = builder.disjunction();
    predicateFilterBy = builder.or(predicateFilterBy, facility.in(desiredUuids));
    predicateFilterBy = builder.or(predicateFilterBy, program.in(desiredUuids));
    //Add first important filter
    Predicate predicate = builder.equal(root.get("status"), RequisitionStatus.APPROVED);
    //Connector filters
    predicate = builder.and(predicate, predicateFilterBy);
    return predicate;
  }

  private Query setPaging(Query query, Integer pageNumber, Integer pageSize) {
    query.setFirstResult((pageNumber - 1) * pageSize);
    query.setMaxResults(pageSize);
    return query;
  }

  private List<UUID> findDesiredUuids(String filterValue, String filterBy) {
    List<UUID> uuidsToReturn = new ArrayList<>();

    if (filterBy.equals("programName") || filterBy.equals("all")) {
      Collection<ProgramDto> foundPrograms =
          programReferenceDataService.search(filterValue);
      foundPrograms.forEach(program -> uuidsToReturn.add(program.getId()));
    }
    if (filterBy.equals("facilityCode") || filterBy.equals("all")) {
      Collection<FacilityDto> foundFacilities =
          facilityReferenceDataService.search(filterValue, null);
      foundFacilities.forEach(facilityDto -> uuidsToReturn.add(facilityDto.getId()));
    }
    if (filterBy.equals("facilityName") || filterBy.equals("all")) {
      Collection<FacilityDto> foundFacilities =
          facilityReferenceDataService.search(null, filterValue);
      foundFacilities.forEach(facilityDto -> {
        if (!uuidsToReturn.add(facilityDto.getId())) {
          uuidsToReturn.add(facilityDto.getId());
        }
      });
    }
    return uuidsToReturn;
  }

  private List<RequisitionDto> convertRequisitionListToRequisitionDtoList(
      List<Requisition> requisitions) {
    List<RequisitionDto> requisitionsConvertedToDto = new ArrayList<>();

    for (Requisition requisition : requisitions) {
      RequisitionDto requisitionDto = new RequisitionDto();
      requisitionDto.setId(requisition.getId());
      requisitionDto.setRequsitionLineItems(requisition.getRequisitionLineItems());
      requisitionDto.setComments(requisition.getComments());
      requisitionDto.setStatus(requisition.getStatus());
      requisitionDto.setEmergency(requisition.getEmergency());
      requisitionDto.setSupervisoryNode(requisition.getSupervisoryNodeId());
      requisitionDto.setSupplyingFacility(requisition.getSupplyingFacilityId());
      FacilityDto facilityDto = facilityReferenceDataService.findOne(requisition.getFacilityId());
      requisitionDto.setFacility(facilityDto);
      ProgramDto programDto = programReferenceDataService.findOne(requisition.getProgramId());
      requisitionDto.setProgram(programDto);
      ProcessingPeriodDto processingPeriodDto =
          processingPeriodReferenceDataService.findOne(requisition.getProcessingPeriodId());
      requisitionDto.setProcessingPeriod(processingPeriodDto);
      requisitionsConvertedToDto.add(requisitionDto);
    }

    return requisitionsConvertedToDto;
  }

}
