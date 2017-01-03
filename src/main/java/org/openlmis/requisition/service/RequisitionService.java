package org.openlmis.requisition.service;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionBuilder;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.DetailedRoleAssignmentDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.InvalidRequisitionStateException;
import org.openlmis.requisition.exception.InvalidRequisitionStatusException;
import org.openlmis.requisition.exception.RequisitionConversionException;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.exception.RequisitionNotFoundException;
import org.openlmis.requisition.exception.RequisitionTemplateNotFoundException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.fulfillment.OrderFulfillmentService;
import org.openlmis.requisition.service.referencedata.ApprovedProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.settings.exception.ConfigurationSettingException;
import org.openlmis.requisition.service.referencedata.UserRoleAssignmentsReferenceDataService;
import org.openlmis.utils.ConvertHelper;
import org.openlmis.utils.Message;
import org.openlmis.utils.PaginationHelper;
import org.openlmis.utils.RequisitionDtoComparator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
// TODO: split this up in OLMIS-1102
@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionService {
  private static final String REQUISITION_BAD_STATUS_MESSAGE = "requisition has bad status";
  private static final String CAN_NOT_SKIP_PERIOD_STATUS =
      "requisition.error.can-not-skip-period.status";
  private static final String CAN_NOT_SKIP_PERIOD_PROGRAM =
      "requisition.error.can-not-skip-period.program";

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionService.class);

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private RequisitionTemplateService requisitionTemplateService;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private PeriodService periodService;

  @Autowired
  private ApprovedProductReferenceDataService approvedProductReferenceDataService;

  @Autowired
  private UserFulfillmentFacilitiesReferenceDataService fulfillmentFacilitiesReferenceDataService;

  @Autowired
  private UserRoleAssignmentsReferenceDataService roleAssignmentsReferenceDataService;

  @Autowired
  private OrderFulfillmentService orderFulfillmentService;

  @Autowired
  private PaginationHelper paginationHelper;

  @Autowired
  private ConvertHelper convertHelper;

  @Autowired
  private RequisitionStatusNotifier requisitionStatusNotifier;

  /**
   * Initiated given requisition if possible.
   *
   * @param programId         UUID of Program.
   * @param facilityId        UUID of Facility.
   * @param creatorId         UUID of the user that initiates the requisition.
   * @param emergency         Emergency status.
   * @param suggestedPeriodId Period for requisition.
   * @return Initiated requisition.
   * @throws RequisitionException Exception thrown when it is not possible to initialize a
   *                              requisition.
   */
  public Requisition initiate(UUID programId, UUID facilityId, UUID suggestedPeriodId,
                              UUID creatorId, boolean emergency)
      throws RequisitionException {
    Requisition requisition = RequisitionBuilder.newRequisition(facilityId, programId, emergency);
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setCreatorId(creatorId);

    ProcessingPeriodDto period = periodService
        .findPeriod(programId, facilityId, suggestedPeriodId, emergency);

    requisition.setProcessingPeriodId(period.getId());
    requisition.setNumberOfMonthsInPeriod(period.getDurationInMonths());

    FacilityDto facility = facilityReferenceDataService.findOne(facilityId);
    ProgramDto program = programReferenceDataService.findOne(programId);

    Collection<ApprovedProductDto> approvedProducts =
        approvedProductReferenceDataService.getApprovedProducts(
            facility.getId(), program.getId(), true
        );
    RequisitionTemplate requisitionTemplate = findRequisitionTemplate(programId);
    Requisition previousRequisition = findPreviousRequisition(requisition);

    requisition.initiate(requisitionTemplate, approvedProducts,
        previousRequisition);

    requisitionRepository.save(requisition);
    return requisition;
  }

  private RequisitionTemplate findRequisitionTemplate(UUID programId) throws RequisitionException {
    if (null == programId) {
      throw new IllegalArgumentException("Program ID cannot be null");
    }

    RequisitionTemplate template =
        requisitionTemplateService.getTemplateForProgram(programId);

    if (null == template) {
      throw new RequisitionTemplateNotFoundException("RequisitionTemplate not found");
    }

    if (!template.hasColumnsDefined()) {
      throw new RequisitionTemplateNotFoundException("RequisitionTemplate is not defined");
    } else {
      return template;
    }
  }

  /**
   * Delete given Requisition if possible.
   *
   * @param requisitionId UUID of Requisition to be deleted.
   * @throws RequisitionException Exception thrown when it is not possible to delete a requisition.
   */
  public void delete(UUID requisitionId) throws RequisitionException {
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (requisition == null) {
      throw new RequisitionNotFoundException(requisitionId);
    } else if (requisition.getStatus() != RequisitionStatus.INITIATED) {
      throw new InvalidRequisitionStatusException("Delete failed - "
          + REQUISITION_BAD_STATUS_MESSAGE);
    } else {
      requisitionRepository.delete(requisition);
      LOGGER.debug("Requisition deleted");
    }
  }

  /**
   * Skip given requisition if possible.
   *
   * @param requisitionId UUID of Requisition to be skipped.
   * @return Skipped Requisition.
   * @throws RequisitionException Exception thrown when it is not possible to skip a requisition.
   */
  public Requisition skip(UUID requisitionId) throws RequisitionException {
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (requisition == null) {
      throw new RequisitionNotFoundException(requisitionId);
    } else {
      ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());
      if (requisition.getStatus() != RequisitionStatus.INITIATED) {
        throw new ValidationMessageException(new Message(CAN_NOT_SKIP_PERIOD_STATUS));
      } else if (!program.getPeriodsSkippable()) {
        throw new ValidationMessageException(new Message(CAN_NOT_SKIP_PERIOD_PROGRAM));
      } else {
        LOGGER.debug("Requisition skipped");

        for (RequisitionLineItem item : requisition.getRequisitionLineItems()) {
          item.skipLineItem(requisition.getTemplate());
        }
        requisition.setStatus(RequisitionStatus.SKIPPED);
        return requisitionRepository.save(requisition);
      }
    }
  }

  /**
   * Reject given requisition if possible.
   *
   * @param requisitionId UUID of Requisition to be rejected.
   * @throws RequisitionException Exception thrown when it is not possible to reject a requisition.
   */
  public Requisition reject(UUID requisitionId) throws RequisitionException {

    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      throw new RequisitionNotFoundException(requisitionId);
    } else if (requisition.getStatus() == RequisitionStatus.AUTHORIZED) {
      LOGGER.debug("Requisition rejected: " + requisitionId);
      requisition.setStatus(RequisitionStatus.INITIATED);
      return requisitionRepository.save(requisition);
    } else {
      throw new InvalidRequisitionStatusException("Cannot reject requisition: " + requisitionId
          + " .Requisition must be waiting for approval to be rejected");
    }
  }

  /**
   * Finds requisitions matching all of provided parameters.
   */
  public List<Requisition> searchRequisitions(UUID facility, UUID program,
                                              LocalDateTime createdDateFrom,
                                              LocalDateTime createdDateTo,
                                              UUID processingPeriod,
                                              UUID supervisoryNode,
                                              RequisitionStatus[] requisitionStatuses,
                                              Boolean emergency) {
    return requisitionRepository.searchRequisitions(facility, program, createdDateFrom,
        createdDateTo, processingPeriod, supervisoryNode, requisitionStatuses, emergency);
  }

  /**
   * Finds requisitions matching all of provided parameters.
   */
  public List<Requisition> searchRequisitions(UUID facility, UUID program, UUID processingPeriod,
                                              UUID supervisoryNode) {
    return requisitionRepository.searchRequisitions(facility, program, null, null, processingPeriod,
        supervisoryNode, null, null);
  }

  /**
   * Get requisitions to approve for specified user.
   */
  public List<Requisition> getRequisitionsForApproval(UUID userId) {
    List<Requisition> requisitionsForApproval = new ArrayList<>();
    Collection<DetailedRoleAssignmentDto> roleAssignments =
        roleAssignmentsReferenceDataService.getRoleAssignments(userId);
    if (roleAssignments != null) {
      for (DetailedRoleAssignmentDto roleAssignment : roleAssignments) {
        if (roleAssignment.getSupervisoryNodeId() != null
            && roleAssignment.getProgramId() != null) {
          requisitionsForApproval.addAll(getAuthorizedRequisitions(
              roleAssignment.getProgramId(), roleAssignment.getSupervisoryNodeId()));
        }
      }
    }
    return requisitionsForApproval;
  }

  /**
   * Get authorized requisitions for specified program.
   */
  public List<Requisition> getAuthorizedRequisitions(UUID programId,
                                                     UUID supervisoryNodeId) {
    List<Requisition> requisitions = new ArrayList<>();
    List<Requisition> reqList = searchRequisitions(null, programId,
            null, null, null, supervisoryNodeId, null, null);
    if (reqList != null) {
      for (Requisition req : reqList) {
        if (req.getStatus() == RequisitionStatus.AUTHORIZED) {
          requisitions.add(req);
        }
      }
    }
    return requisitions;
  }

  /**
   * Releases the list of given requisitions as order.
   *
   * @param convertToOrderDtos list of Requisitions with their supplyingDepots to be released as
   *                           order
   * @return list of released requisitions
   */
  public List<Requisition> releaseRequisitionsAsOrder(
      List<ConvertToOrderDto> convertToOrderDtos, UserDto user) throws RequisitionException {
    List<Requisition> releasedRequisitions = new ArrayList<>();
    Set<UUID> userFacilities = fulfillmentFacilitiesReferenceDataService
        .getFulfillmentFacilities(user.getId()).stream().map(FacilityDto::getId)
        .collect(Collectors.toSet());

    for (ConvertToOrderDto convertToOrderDto : convertToOrderDtos) {
      UUID requisitionId = convertToOrderDto.getRequisitionId();
      Requisition loadedRequisition = requisitionRepository.findOne(requisitionId);

      if (RequisitionStatus.APPROVED == loadedRequisition.getStatus()) {
        loadedRequisition.setStatus(RequisitionStatus.RELEASED);
      } else {
        throw new InvalidRequisitionStatusException("Can not release requisition: "
            + loadedRequisition.getId() + " as order. Requisition must be approved.");
      }

      UUID facilityId = convertToOrderDto.getSupplyingDepotId();
      Set<UUID> validFacilities = getAvailableSupplyingDepots(requisitionId)
          .stream().filter(f -> userFacilities.contains(f.getId())).map(FacilityDto::getId)
          .collect(Collectors.toSet());

      if (validFacilities.contains(facilityId)) {
        loadedRequisition.setSupplyingFacilityId(facilityId);
      } else {
        throw new InvalidRequisitionStateException("Can not release requisition: "
            + loadedRequisition.getId() + " as order. Requisition must have supplying facility.");
      }

      releasedRequisitions.add(loadedRequisition);
    }

    return releasedRequisitions;
  }

  /**
   * Retrieves available supplying depots for given requisition.
   *
   * @param requisitionId id of requisition to find facilities for
   * @return list of facilities
   */
  public List<FacilityDto> getAvailableSupplyingDepots(UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    Collection<FacilityDto> facilityDtos = facilityReferenceDataService
        .searchSupplyingDepots(requisition.getProgramId(), requisition.getSupervisoryNodeId());
    return new ArrayList<>(facilityDtos);
  }

  /**
   * Get approved requisitions matching all of provided parameters.
   *
   * @param filterValue Value to be used to filter.
   * @param filterBy    Field used to filter: "programName", "facilityCode", "facilityName" or
   *                    "all".
   * @param sortBy      Field used to sort: "programName", "facilityCode" or "facilityName".
   * @param descending  Descending direction for sort.
   * @param pageNumber  Page number to return.
   * @param pageSize    Quantity for one page.
   * @return List of requisitions.
   */
  public List<RequisitionDto> searchApprovedRequisitionsWithSortAndFilterAndPaging(
      String filterValue, String filterBy, String sortBy, Boolean descending,
      Integer pageNumber, Integer pageSize) {

    List<UUID> desiredUuids = findDesiredUuids(filterValue, filterBy);
    List<Requisition> requisitions =
        requisitionRepository.searchApprovedRequisitions(filterBy, desiredUuids);
    List<RequisitionDto> requisitionDtos = convertHelper
        .convertRequisitionListToRequisitionDtoList(requisitions);

    requisitionDtos.sort(new RequisitionDtoComparator(sortBy));
    if (descending) {
      Collections.reverse(requisitionDtos);
    }
    if (pageNumber != null && pageSize != null) {
      requisitionDtos = paginationHelper.pageCollection(requisitionDtos, pageNumber,
          pageSize);
    }

    return requisitionDtos;
  }

  /**
   * Converting Requisition list to Orders.
   */
  @Transactional
  public void convertToOrder(List<ConvertToOrderDto> list, UserDto user)
      throws RequisitionException, ConfigurationSettingException {
    List<Requisition> releasedRequisitions = releaseRequisitionsAsOrder(list, user);

    for (Requisition requisition : releasedRequisitions) {
      OrderDto order = OrderDto.newOrder(requisition, user);
      if (orderFulfillmentService.create(order)) {
        requisitionRepository.save(requisition);
        requisitionStatusNotifier.notifyConvertToOrder(user, requisition);
      } else {
        throw new RequisitionConversionException("Error while converting requisition: "
            + order.getExternalId() + " to order.");
      }
    }
  }

  private List<UUID> findDesiredUuids(String filterValue, String filterBy) {
    List<UUID> uuidsToReturn = new ArrayList<>();

    boolean filterAll = "all".equals(filterBy);

    if (filterAll || "programName".equalsIgnoreCase(filterBy)) {
      Collection<ProgramDto> foundPrograms =
          programReferenceDataService.search(filterValue);
      foundPrograms.forEach(programDto -> uuidsToReturn.add(programDto.getId()));
    }
    if (filterAll || "facilityCode".equals(filterBy)) {
      Collection<FacilityDto> foundFacilities =
          facilityReferenceDataService.search(filterValue, null);
      foundFacilities.forEach(facilityDto -> uuidsToReturn.add(facilityDto.getId()));
    }
    if (filterAll || "facilityName".equals(filterBy)) {
      Collection<FacilityDto> foundFacilities =
          facilityReferenceDataService.search(null, filterValue);
      foundFacilities.forEach(facilityDto -> uuidsToReturn.add(facilityDto.getId()));
    }
    return uuidsToReturn;
  }

  private Requisition findPreviousRequisition(Requisition currentRequisition) {
    // ... we try to find previous period and requisition ...
    ProcessingPeriodDto previousPeriod = periodService.findPreviousPeriod(
        currentRequisition.getProcessingPeriodId()
    );
    return null != previousPeriod
        ? findPreviousRequisition(currentRequisition, previousPeriod)
        : null;
  }

  private Requisition findPreviousRequisition(Requisition currentRequisition,
                                              ProcessingPeriodDto previousPeriod) {
    List<Requisition> list = searchRequisitions(
        currentRequisition.getFacilityId(), currentRequisition.getProgramId(),
        null, null, previousPeriod.getId(), null, null, null
    );

    return null == list ? null : (list.isEmpty() ? null : list.get(0));
  }
}
