package org.openlmis.requisition.service;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionBuilder;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProductDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.openlmis.requisition.service.fulfillment.OrderFulfillmentService;
import org.openlmis.requisition.service.referencedata.ApprovedProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.RightReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserSupervisedFacilitiesReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserSupervisedProgramsReferenceDataService;
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
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
// TODO: split this up in OLMIS-1102
@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionService {

  private static final String CAN_NOT_SKIP_PERIOD_STATUS =
      "requisition.error.canNotSkipPeriod.status";
  private static final String CAN_NOT_SKIP_PERIOD_PROGRAM =
      "requisition.error.canNotSkipPeriod.program";
  private static final String CAN_NOT_SKIP_EMERGENCY_REQUISITION =
      "requisition.error.canNotSkipPeriod.emergency";
  private static final String REQUISITION_APPROVE = "REQUISITION_APPROVE";
  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionService.class);
  private static final String REQUISITION_NOT_FOUND = "requisition.error.requisition-not-found";

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private StatusMessageRepository statusMessageRepository;

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
  private UserSupervisedFacilitiesReferenceDataService supervisedFacilitiesReferenceDataService;

  @Autowired
  private UserSupervisedProgramsReferenceDataService userProgramsReferenceDataService;

  @Autowired
  private RightReferenceDataService rightReferenceDataService;

  @Autowired
  private OrderableProductReferenceDataService orderableProductReferenceDataService;

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
   */
  public Requisition initiate(UUID programId, UUID facilityId, UUID suggestedPeriodId,
                              UUID creatorId, boolean emergency) {
    Requisition requisition = RequisitionBuilder.newRequisition(
        facilityId, programId, creatorId, emergency);
    requisition.setStatus(RequisitionStatus.INITIATED);

    ProcessingPeriodDto period = periodService
        .findPeriod(programId, facilityId, suggestedPeriodId, emergency);

    requisition.setProcessingPeriodId(period.getId());
    requisition.setNumberOfMonthsInPeriod(period.getDurationInMonths());

    FacilityDto facility = facilityReferenceDataService.findOne(facilityId);
    ProgramDto program = programReferenceDataService.findOne(programId);

    Collection<ApprovedProductDto> approvedProducts =
        approvedProductReferenceDataService.getApprovedProducts(
            facility.getId(), program.getId(), true);

    RequisitionTemplate requisitionTemplate = findRequisitionTemplate(programId);

    int numberOfPreviousPeriodsToAverage;
    List<Requisition> previousRequisitions;
    // numberOfPeriodsToAverage is always >= 2 or null
    if (requisitionTemplate.getNumberOfPeriodsToAverage() == null) {
      numberOfPreviousPeriodsToAverage = 0;
      previousRequisitions = getRecentRequisitions(requisition, 1);
    } else {
      numberOfPreviousPeriodsToAverage = requisitionTemplate.getNumberOfPeriodsToAverage() - 1;
      previousRequisitions =
          getRecentRequisitions(requisition, numberOfPreviousPeriodsToAverage);
    }

    if (numberOfPreviousPeriodsToAverage > previousRequisitions.size()) {
      numberOfPreviousPeriodsToAverage = previousRequisitions.size();
    }
    requisition.initiate(requisitionTemplate, approvedProducts, previousRequisitions,
        numberOfPreviousPeriodsToAverage);

    requisitionRepository.save(requisition);
    return requisition;
  }

  private RequisitionTemplate findRequisitionTemplate(UUID programId) {
    if (null == programId) {
      throw new ValidationMessageException(new Message(
          "requisition.error.program-id-cannot-be-null"));
    }

    RequisitionTemplate template =
        requisitionTemplateService.getTemplateForProgram(programId);

    if (null == template) {
      throw new ContentNotFoundMessageException(new Message(
          "requisition.error.requisition-template-not-found"));
    }

    if (!template.hasColumnsDefined()) {
      throw new ValidationMessageException(new Message(
          "requisition.error.requisitiontemplate-not-defined"));
    } else {
      return template;
    }
  }

  /**
   * Delete given Requisition if possible.
   *
   * @param requisitionId UUID of Requisition to be deleted.
   */
  public void delete(UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (requisition == null) {
      throw new ContentNotFoundMessageException(new Message(
          REQUISITION_NOT_FOUND, requisitionId));
    } else if (requisition.getStatus() != RequisitionStatus.INITIATED) {
      throw new ValidationMessageException(new Message("requisition.error.bad-status.delete"));
    } else {
      statusMessageRepository.delete(statusMessageRepository.findByRequisitionId(requisitionId));
      requisitionRepository.delete(requisition);
      LOGGER.debug("Requisition deleted");
    }
  }

  /**
   * Skip given requisition if possible.
   *
   * @param requisitionId UUID of Requisition to be skipped.
   * @return Skipped Requisition.
   */
  public Requisition skip(UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (requisition == null) {
      throw new ContentNotFoundMessageException(new Message(
          REQUISITION_NOT_FOUND, requisitionId));
    } else {
      ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());
      if (requisition.getStatus() != RequisitionStatus.INITIATED) {
        throw new ValidationMessageException(new Message(CAN_NOT_SKIP_PERIOD_STATUS));
      } else if (!program.getPeriodsSkippable()) {
        throw new ValidationMessageException(new Message(CAN_NOT_SKIP_PERIOD_PROGRAM));
      } else if (requisition.getEmergency()) {
        throw new ValidationMessageException(new Message(CAN_NOT_SKIP_EMERGENCY_REQUISITION));
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
   */
  public Requisition reject(UUID requisitionId) {

    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      throw new ContentNotFoundMessageException(new Message(
          REQUISITION_NOT_FOUND, requisitionId));
    } else if (requisition.getStatus() == RequisitionStatus.AUTHORIZED) {
      LOGGER.debug("Requisition rejected: " + requisitionId);
      requisition.setStatus(RequisitionStatus.INITIATED);
      return requisitionRepository.save(requisition);
    } else {
      throw new ValidationMessageException(new Message(
          "requisition.error.reject.requisition-must-be-waiting-for-approval", requisitionId));
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
  public List<Requisition> searchRequisitions(UUID facility, UUID program, UUID processingPeriod) {
    return requisitionRepository.searchRequisitions(facility, program, null, null, processingPeriod,
        null, null, null);
  }

  /**
   * Get requisitions to approve for specified user.
   */
  public Set<Requisition> getRequisitionsForApproval(UUID userId) {
    Set<Requisition> requisitionsForApproval = new HashSet<>();
    Collection<ProgramDto> programsForUser = getAllProgramsForUser(userId);
    RightDto right = rightReferenceDataService.findRight(REQUISITION_APPROVE);
    for (ProgramDto program : programsForUser) {
      Collection<FacilityDto> supervisedFacilities = supervisedFacilitiesReferenceDataService
          .getFacilitiesSupervisedByUser(userId, program.getId(), right.getId());
      requisitionsForApproval.addAll(addAuthorizedRequisitions(supervisedFacilities, program));
    }

    return requisitionsForApproval;
  }

  private Set<ProgramDto> getAllProgramsForUser(UUID userId) {
    Collection<ProgramDto> supervisedPrograms = userProgramsReferenceDataService
        .getProgramsSupervisedByUser(userId);
    Collection<ProgramDto> homePrograms = userProgramsReferenceDataService
        .getHomeFacilityProgramsByUser(userId);
    Set<ProgramDto> allPrograms = new HashSet<>(supervisedPrograms);
    allPrograms.addAll(homePrograms);

    return allPrograms;
  }

  private List<Requisition> addAuthorizedRequisitions(Collection<FacilityDto> supervisedFacilities,
                                                      ProgramDto program) {
    List<Requisition> requisitions = new ArrayList<>();
    for (FacilityDto facility : supervisedFacilities) {
      requisitions.addAll(getAuthorizedRequisitions(facility, program));
    }
    return requisitions;
  }

  /**
   * Get authorized requisitions for specified program.
   */
  public List<Requisition> getAuthorizedRequisitions(FacilityDto facility, ProgramDto program) {
    List<Requisition> requisitions = new ArrayList<>();
    List<Requisition> reqList = searchRequisitions(facility.getId(), program.getId(),
        null, null, null, null, null, null);
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
      List<ConvertToOrderDto> convertToOrderDtos, UserDto user) {
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
        throw new ValidationMessageException(new Message(
            "requisition.error.release.requisition-must-be-approved", loadedRequisition.getId()));
      }

      UUID facilityId = convertToOrderDto.getSupplyingDepotId();
      Set<UUID> validFacilities = getAvailableSupplyingDepots(requisitionId)
          .stream().filter(f -> userFacilities.contains(f.getId())).map(FacilityDto::getId)
          .collect(Collectors.toSet());

      if (validFacilities.contains(facilityId)) {
        loadedRequisition.setSupplyingFacilityId(facilityId);
      } else {
        throw new ValidationMessageException(new Message(
            "requisition.error.release.must-have-supplying-facility",loadedRequisition.getId()));
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
   * Retrieves full supply requisition line items for requisition with given id.
   *
   * @param requisitionId id of requisition to find items for.
   * @return list of full supply requisition line items.
   */
  public List<RequisitionLineItem> getFullSupplyItems(UUID requisitionId) {
    return getSupplyItemsBase(requisitionId, true);
  }

  /**
   * Retrieves non-full supply requisition line items for requisition with given id.
   *
   * @param requisitionId id of requisition to find items for.
   * @return list of non-full supply requisition line items.
   */
  public List<RequisitionLineItem> getNonFullSupplyItems(UUID requisitionId) {
    return getSupplyItemsBase(requisitionId, false);
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
  public void convertToOrder(List<ConvertToOrderDto> list, UserDto user) {
    List<Requisition> releasedRequisitions = releaseRequisitionsAsOrder(list, user);

    for (Requisition requisition : releasedRequisitions) {
      OrderDto order = OrderDto.newOrder(requisition, user);
      if (orderFulfillmentService.create(order)) {
        requisitionRepository.save(requisition);
        requisitionStatusNotifier.notifyConvertToOrder(requisition);
      } else {
        throw new ValidationMessageException(new Message(
            "requisition.error.converting-requisition-to-order", order.getExternalId()));
      }
    }
  }

  private List<RequisitionLineItem> getSupplyItemsBase(UUID requisitionId, boolean fullSupply) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();

    for (RequisitionLineItem lineItem : requisition.getRequisitionLineItems()) {
      OrderableProductDto orderableProduct = orderableProductReferenceDataService
          .findOne(lineItem.getOrderableProductId());

      Optional<ProductDto> product = orderableProduct.getPrograms().stream()
          .filter(e -> requisition.getProgramId().equals(e.getProgramId())).findFirst();

      product.ifPresent(p -> {
        if (p.getFullSupply() == fullSupply) {
          requisitionLineItems.add(lineItem);
        }
      });
    }

    return requisitionLineItems;
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

  private List<Requisition> getRecentRequisitions(Requisition requisition, int amount) {
    List<ProcessingPeriodDto> previousPeriods =
        periodService.findPreviousPeriods(requisition.getProcessingPeriodId(), amount);

    List<Requisition> recentRequisitions = new ArrayList<>();
    for (ProcessingPeriodDto period : previousPeriods) {
      List<Requisition> requisitionsByPeriod = getRequisitionsByPeriod(requisition, period);
      if (!requisitionsByPeriod.isEmpty()) {
        Requisition requisitionByPeriod = requisitionsByPeriod.get(0);
        recentRequisitions.add(requisitionByPeriod);
      }
    }
    return recentRequisitions;
  }

  private List<Requisition> getRequisitionsByPeriod(Requisition requisition,
                                                    ProcessingPeriodDto period) {
    return searchRequisitions(requisition.getFacilityId(),
        requisition.getProgramId(), period.getId());
  }
}
