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

package org.openlmis.requisition.service;

import static java.util.Objects.isNull;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.openlmis.requisition.domain.RequisitionLineItem.APPROVED_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionStatus.APPROVED;
import static org.openlmis.requisition.domain.RequisitionStatus.SKIPPED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_CANNOT_UPDATE_WITH_STATUS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DELETE_FAILED_WRONG_STATUS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_HAVE_SUPPLYING_FACILITY;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PROGRAM_DOES_NOT_ALLOW_SKIP;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PROGRAM_ID_CANNOT_BE_NULL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_MUST_BE_APPROVED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_MUST_BE_WAITING_FOR_APPROVAL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_TEMPLATE_NOT_DEFINED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_TEMPLATE_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SKIP_FAILED_EMERGENCY;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SKIP_FAILED_WRONG_STATUS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_CANNOT_CONVERT_WITHOUT_APPROVED_QTY;
import static org.springframework.util.CollectionUtils.isEmpty;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionBuilder;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.StatusMessage;
import org.openlmis.requisition.domain.StockAdjustmentReason;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.DetailedRoleAssignmentDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.MinimalFacilityDto;
import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
import org.openlmis.requisition.dto.ProofOfDeliveryDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.openlmis.requisition.service.fulfillment.OrderFulfillmentService;
import org.openlmis.requisition.service.referencedata.ApprovedProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.RightReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserRoleAssignmentsReferenceDataService;
import org.openlmis.requisition.settings.service.ConfigurationSettingService;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.utils.Pagination;
import org.openlmis.requisition.utils.RequisitionForConvertComparator;
import org.openlmis.requisition.utils.RightName;
import org.openlmis.requisition.web.OrderDtoBuilder;
import org.openlmis.requisition.web.RequisitionForConvertBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.profiler.Profiler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
// TODO: split this up in OLMIS-1102
@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionService {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionService.class);

  @Autowired
  private ConfigurationSettingService configurationSettingService;

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
  private OrderableReferenceDataService orderableReferenceDataService;

  @Autowired
  private OrderFulfillmentService orderFulfillmentService;

  @Autowired
  private AuthenticationHelper authenticationHelper;

  @Autowired
  private OrderDtoBuilder orderDtoBuilder;

  @Autowired
  private UserRoleAssignmentsReferenceDataService userRoleAssignmentsReferenceDataService;

  @Autowired
  private RightReferenceDataService rightReferenceDataService;

  @Autowired
  private RequisitionStatusProcessor requisitionStatusProcessor;

  @Autowired
  private ProofOfDeliveryService proofOfDeliveryService;

  @Autowired
  private RequisitionForConvertBuilder requisitionForConvertBuilder;

  @Autowired
  private PermissionService permissionService;

  /**
   * Initiated given requisition if possible.
   *
   * @param programId         UUID of Program.
   * @param facilityId        UUID of Facility.
   * @param suggestedPeriodId Period for requisition.
   * @param emergency         Emergency status.
   * @param stockAdjustmentReasons list of stockAdjustmentReasons
   * @return Initiated requisition.
   */
  public Requisition initiate(UUID programId, UUID facilityId, UUID suggestedPeriodId,
                              boolean emergency,
                              List<StockAdjustmentReason> stockAdjustmentReasons) {
    Requisition requisition = RequisitionBuilder.newRequisition(
        facilityId, programId, emergency);
    requisition.setStatus(RequisitionStatus.INITIATED);

    ProcessingPeriodDto period = periodService
        .findPeriod(programId, facilityId, suggestedPeriodId, emergency);

    requisition.setProcessingPeriodId(period.getId());
    requisition.setNumberOfMonthsInPeriod(period.getDurationInMonths());

    Collection<ApprovedProductDto> approvedProducts =
        approvedProductReferenceDataService.getApprovedProducts(
            facilityId, programId, true);

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

    ProofOfDeliveryDto pod = getProofOfDeliveryDto(emergency, requisition);

    requisition.initiate(requisitionTemplate, approvedProducts, previousRequisitions,
        numberOfPreviousPeriodsToAverage, pod, authenticationHelper.getCurrentUser().getId());

    requisition.setAvailableNonFullSupplyProducts(approvedProductReferenceDataService
        .getApprovedProducts(facilityId, programId, false)
        .stream()
        .map(ap -> ap.getOrderable().getId())
        .collect(Collectors.toSet()));

    requisition.setStockAdjustmentReasons(stockAdjustmentReasons);

    requisitionRepository.save(requisition);
    return requisition;
  }

  private ProofOfDeliveryDto getProofOfDeliveryDto(boolean emergency, Requisition requisition) {
    List<Requisition> previous = getRecentRequisitions(requisition, 1);
    ProofOfDeliveryDto pod = null;

    if (!emergency && !isEmpty(previous)) {
      pod = proofOfDeliveryService.get(previous.get(0));
    }

    return pod;
  }

  private RequisitionTemplate findRequisitionTemplate(UUID programId) {
    if (null == programId) {
      throw new ValidationMessageException(new Message(ERROR_PROGRAM_ID_CANNOT_BE_NULL));
    }

    RequisitionTemplate template =
        requisitionTemplateService.getTemplateForProgram(programId);

    if (null == template) {
      throw new ContentNotFoundMessageException(new Message(ERROR_REQUISITION_TEMPLATE_NOT_FOUND));
    }

    if (!template.hasColumnsDefined()) {
      throw new ValidationMessageException(new Message(ERROR_REQUISITION_TEMPLATE_NOT_DEFINED));
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
      throw new ContentNotFoundMessageException(new Message(ERROR_REQUISITION_NOT_FOUND,
          requisitionId));
    } else if (!requisition.isPreAuthorize()) {
      throw new ValidationMessageException(new Message(ERROR_DELETE_FAILED_WRONG_STATUS));
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
      throw new ContentNotFoundMessageException(new Message(ERROR_REQUISITION_NOT_FOUND,
          requisitionId));
    } else {
      ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());
      if (!requisition.getStatus().isSubmittable()) {
        throw new ValidationMessageException(new Message(ERROR_SKIP_FAILED_WRONG_STATUS));
      } else if (!program.getPeriodsSkippable()) {
        throw new ValidationMessageException(new Message(ERROR_PROGRAM_DOES_NOT_ALLOW_SKIP));
      } else if (requisition.getEmergency()) {
        throw new ValidationMessageException(new Message(ERROR_SKIP_FAILED_EMERGENCY));
      } else {
        LOGGER.debug("Requisition skipped");

        for (RequisitionLineItem item : requisition.getRequisitionLineItems()) {
          item.skipLineItem(requisition.getTemplate());
        }
        requisition.setStatus(SKIPPED);
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
      throw new ContentNotFoundMessageException(new Message(ERROR_REQUISITION_NOT_FOUND,
          requisitionId));
    } else if (requisition.isApprovable()) {
      UUID userId = authenticationHelper.getCurrentUser().getId();
      validateCanApproveRequisition(requisition, requisitionId, userId).throwExceptionIfHasErrors();

      LOGGER.debug("Requisition rejected: {}", requisitionId);
      Set<UUID> orderableIds = requisition.getRequisitionLineItems().stream().map(
              RequisitionLineItem::getOrderableId).collect(Collectors.toSet());
      requisition.reject(orderableReferenceDataService.findByIds(orderableIds), userId);
      saveStatusMessage(requisition);
      return requisitionRepository.save(requisition);
    } else {
      throw new ValidationMessageException(new Message(
          ERROR_REQUISITION_MUST_BE_WAITING_FOR_APPROVAL, requisitionId));
    }
  }
  
  /**
   * Finds requisitions matching all of the provided parameters.
   */
  public Page<Requisition> searchRequisitions(UUID facility, UUID program,
                                              LocalDate initiatedDateFrom,
                                              LocalDate initiatedDateTo,
                                              UUID processingPeriod,
                                              UUID supervisoryNode,
                                              Set<RequisitionStatus> requisitionStatuses,
                                              Boolean emergency,
                                              Pageable pageable) {
    Profiler profiler = new Profiler("REQUISITION_SERVICE_SEARCH");
    profiler.setLogger(LOGGER);

    profiler.start("GET_PERM_STRINGS");
    List<String> permissionStrings = permissionService.getPermissionStrings();
    if (permissionStrings.isEmpty()) {
      profiler.stop().log();
      return Pagination.getPage(Collections.emptyList(), pageable);
    }
    
    profiler.start("REPOSITORY_SEARCH");
    Page<Requisition> results = requisitionRepository.searchRequisitions(facility, program, 
        initiatedDateFrom, initiatedDateTo, processingPeriod, supervisoryNode, 
        requisitionStatuses, emergency, permissionStrings, pageable);

    profiler.stop().log();
    return results;
  }

  /**
   * Finds requisitions matching all of the provided parameters.
   */
  public Page<Requisition> searchRequisitions(UUID facility,
                                              UUID program,
                                              UUID processingPeriod,
                                              Pageable pageable) {
    return requisitionRepository.searchRequisitions(facility, program, null, null,
        processingPeriod, null, null, null, permissionService.getPermissionStrings(), pageable);
  }

  /**
   * Finds requisitions matching all of the provided parameters.
   */
  public Page<Requisition> searchRequisitions(Set<RequisitionStatus> requisitionStatuses,
                                              Pageable pageable) {
    return requisitionRepository.searchRequisitions(null, null, null, null, null,
        null, requisitionStatuses, null, permissionService.getPermissionStrings(), pageable);
  }

  /**
   * Finds requisitions matching supervisory node and program.
   */
  public Page<Requisition> searchRequisitions(UUID program,
                                              UUID supervisoryNode,
                                              Pageable pageable) {
    return requisitionRepository.searchRequisitions(null, program, null, null, null,
        supervisoryNode, null, null, permissionService.getPermissionStrings(), pageable);
  }

  /**
   * Get requisitions to approve for the specified user.
   */
  public List<Requisition> getRequisitionsForApproval(UUID userId, UUID programId) {
    List<Requisition> requisitionsForApproval = new ArrayList<>();
    RightDto right = rightReferenceDataService.findRight(RightName.REQUISITION_APPROVE);
    List<DetailedRoleAssignmentDto> roleAssignments = userRoleAssignmentsReferenceDataService
        .getRoleAssignments(userId)
        .stream()
        .filter(r -> r.getRole().getRights().contains(right))
        .collect(Collectors.toList());

    if (roleAssignments != null) {
      Set<Pair> programNodePairs = new HashSet<>();
      for (DetailedRoleAssignmentDto roleAssignment : roleAssignments) {
        if (roleAssignment.getSupervisoryNodeId() != null
            && roleAssignment.getProgramId() != null
                && (programId == null || programId.equals(roleAssignment.getProgramId()))) {
          programNodePairs.add(new ImmutablePair<>(
              roleAssignment.getProgramId(), roleAssignment.getSupervisoryNodeId()));
        }
      }
      requisitionsForApproval = requisitionRepository
          .searchApprovableRequisitionsByProgramSupervisoryNodePairs(programNodePairs);
    }
    return requisitionsForApproval;
  }

  /**
   * Get approvable requisitions for specified program and supervisoryNode.
   */
  public List<Requisition> getApprovableRequisitions(UUID programId,
                                                     UUID supervisoryNodeId) {
    Page<Requisition> requisitions = searchRequisitions(programId, supervisoryNodeId,
        new PageRequest(Pagination.DEFAULT_PAGE_NUMBER, Pagination.NO_PAGINATION));

    List<Requisition> filteredRequisitions = new ArrayList<>();
    if (requisitions != null) {
      for (Requisition requisition : requisitions) {
        if (requisition.isApprovable()) {
          filteredRequisitions.add(requisition);
        }
      }
    }
    return filteredRequisitions;
  }

  /**
   * Performs several validation checks to ensure that the given requisition can be approved.
   * It makes sure that the user has got rights to approve the requisition, that the requisition
   * exists and that it has got correct status to be eligible for approval.
   *
   * @param requisition the requisition to verify
   * @param requisitionId requisition ID for which the request was made
   * @param userId the UUID of the user approving the requisition
   * @return ValidationResult instance containing the outcome of this validation
   */
  public ValidationResult validateCanApproveRequisition(Requisition requisition, UUID requisitionId,
                                                        UUID userId) {

    ValidationResult permissionCheck = permissionService.canApproveRequisition(requisitionId);
    if (permissionCheck.hasErrors()) {
      return permissionCheck;
    }

    if (requisition == null) {
      return ValidationResult.notFound(
          MessageKeys.ERROR_REQUISITION_NOT_FOUND, requisitionId);
    }

    if (configurationSettingService.getSkipAuthorization() ? requisition.getStatus()
        != RequisitionStatus.SUBMITTED : !requisition.isApprovable()) {
      return ValidationResult.failedValidation(MessageKeys
          .ERROR_REQUISITION_MUST_BE_AUTHORIZED_OR_SUBMITTED, requisition.getId());
    }

    RightDto right = rightReferenceDataService.findRight(RightName.REQUISITION_APPROVE);
    if (!userRoleAssignmentsReferenceDataService.hasSupervisionRight(right, userId,
        requisition.getProgramId(), requisition.getSupervisoryNodeId())) {
      return ValidationResult.noPermission(
          MessageKeys.ERROR_NO_PERMISSION_TO_APPROVE_REQUISITION);
    }

    return ValidationResult.success();
  }

  /**
   * Performs several validation checks to ensure that the given requisition can be saved.
   * It makes sure that the user has got rights to save the requisition, that the requisition
   * exists and that it has got correct status to be eligible for saving.
   *
   * @param requisitionId the UUID for which the request was made
   * @return ValidationResult instance containing the outcome of this validation
   */
  public ValidationResult validateCanSaveRequisition(UUID requisitionId) {
    ValidationResult permissionCheck = permissionService.canUpdateRequisition(requisitionId);
    if (permissionCheck.hasErrors()) {
      return permissionCheck;
    }

    Requisition requisitionToUpdate = requisitionRepository.findOne(requisitionId);
    if (isNull(requisitionToUpdate)) {
      return ValidationResult.notFound(ERROR_REQUISITION_NOT_FOUND, requisitionId);
    }

    RequisitionStatus status = requisitionToUpdate.getStatus();
    if (!status.isUpdatable()) {
      return ValidationResult.failedValidation(ERROR_CANNOT_UPDATE_WITH_STATUS,
          requisitionToUpdate.getStatus());
    }

    return ValidationResult.success();
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
    RightDto right = authenticationHelper.getRight(RightName.ORDERS_EDIT);
    List<Requisition> releasedRequisitions = new ArrayList<>();
    Set<UUID> userFacilities = fulfillmentFacilitiesReferenceDataService
        .getFulfillmentFacilities(user.getId(), right.getId()).stream().map(FacilityDto::getId)
        .collect(Collectors.toSet());

    for (ConvertToOrderDto convertToOrderDto : convertToOrderDtos) {
      UUID requisitionId = convertToOrderDto.getRequisitionId();
      Requisition loadedRequisition = requisitionRepository.findOne(requisitionId);
      isEligibleForConvertToOrder(loadedRequisition).throwExceptionIfHasErrors();
      loadedRequisition.release(authenticationHelper.getCurrentUser().getId());

      UUID facilityId = convertToOrderDto.getSupplyingDepotId();
      Set<UUID> validFacilities = requisitionForConvertBuilder
          .getAvailableSupplyingDepots(requisitionId).stream()
          .filter(f -> userFacilities.contains(f.getId())).map(FacilityDto::getId)
          .collect(Collectors.toSet());

      if (validFacilities.contains(facilityId)) {
        loadedRequisition.setSupplyingFacilityId(facilityId);
      } else {
        throw new ValidationMessageException(new Message(ERROR_MUST_HAVE_SUPPLYING_FACILITY,
            loadedRequisition.getId()));
      }

      releasedRequisitions.add(loadedRequisition);
    }

    return releasedRequisitions;
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
   * @param filterValues Expressions to be used in filters.
   * @param filterBy     Field used to filter: "programName", "facilityCode", "facilityName" or
   *                     "all".
   * @param pageable     Pageable object that allows to optionally add "page" (page number)
   *                     and "size" (page size) query parameters.
   * @param userManagedFacilities List of UUIDs of facilities that are managed by logged in user.
   * @return List of requisitions.
   */
  public Page<RequisitionWithSupplyingDepotsDto>
      searchApprovedRequisitionsWithSortAndFilterAndPaging(List<String> filterValues,
                                                           String filterBy,
                                                           Pageable pageable,
                                                           Collection<UUID> userManagedFacilities) {
    List<UUID> desiredUuids = findDesiredUuids(filterValues, filterBy);
    List<Requisition> requisitionsList =
        requisitionRepository.searchApprovedRequisitions(filterBy, desiredUuids);

    List<RequisitionWithSupplyingDepotsDto> responseList =
        requisitionForConvertBuilder.buildRequisitions(requisitionsList, userManagedFacilities);

    responseList.sort(new RequisitionForConvertComparator(pageable));
    return Pagination.getPage(responseList, pageable);
  }

  /**
   * Converting Requisition list to Orders.
   */
  @Transactional
  public void convertToOrder(List<ConvertToOrderDto> list, UserDto user) {
    List<Requisition> releasedRequisitions = releaseRequisitionsAsOrder(list, user);

    List<OrderDto> orders = new ArrayList<>();
    for (Requisition requisition : releasedRequisitions) {
      OrderDto order = orderDtoBuilder.build(requisition, user);
      orders.add(order);

      requisitionRepository.save(requisition);
      requisitionStatusProcessor.statusChange(requisition);
    }

    orderFulfillmentService.create(orders);
  }

  private ValidationResult isEligibleForConvertToOrder(Requisition requisition) {
    if (APPROVED != requisition.getStatus()) {
      return ValidationResult.failedValidation(
          ERROR_REQUISITION_MUST_BE_APPROVED, requisition.getId());
    } else if (!approvedQtyColumnEnabled(requisition)) {
      return ValidationResult.failedValidation(
          ERROR_VALIDATION_CANNOT_CONVERT_WITHOUT_APPROVED_QTY, requisition.getId());
    }
    return ValidationResult.success();
  }

  private boolean approvedQtyColumnEnabled(Requisition requisition) {
    return requisition.getTemplate().isColumnInTemplateAndDisplayed(APPROVED_QUANTITY);
  }

  private List<RequisitionLineItem> getSupplyItemsBase(UUID requisitionId, boolean fullSupply) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();

    for (RequisitionLineItem lineItem : requisition.getRequisitionLineItems()) {
      OrderableDto orderable = orderableReferenceDataService
          .findOne(lineItem.getOrderableId());

      Optional<ProgramOrderableDto> product = orderable.getPrograms().stream()
          .filter(e -> requisition.getProgramId().equals(e.getProgramId())).findFirst();

      product.ifPresent(p -> {
        if (p.getFullSupply() == fullSupply) {
          requisitionLineItems.add(lineItem);
        }
      });
    }

    return requisitionLineItems;
  }

  private List<UUID> findDesiredUuids(List<String> filterValues, String filterBy) {
    List<UUID> uuidsToReturn = new ArrayList<>();
    filterValues = filterValues == null ? Collections.EMPTY_LIST : filterValues;

    Collection<ProgramDto> programs = findProgramsWithFilter(filterBy, filterValues);
    Collection<MinimalFacilityDto> facilities = findFacilitiesWithFilter(filterBy, filterValues);

    facilities.forEach(facilityDto -> uuidsToReturn.add(facilityDto.getId()));
    programs.forEach(programDto -> uuidsToReturn.add(programDto.getId()));

    return uuidsToReturn;
  }

  private Collection<ProgramDto> findProgramsWithFilter(String filterBy,
                                                        List<String> filterValues) {
    boolean filterAll = isFilterAll(filterBy);
    List<ProgramDto> foundPrograms = new ArrayList<>();

    if (filterAll || "programName".equalsIgnoreCase(filterBy)) {
      if (filterValues.isEmpty()) {
        return programReferenceDataService.findAll();
      }

      for (String expression : filterValues) {
        foundPrograms.addAll(programReferenceDataService.search(expression));
      }
    }

    return foundPrograms;
  }

  private Collection<MinimalFacilityDto> findFacilitiesWithFilter(String filterBy,
                                                                  List<String> filterValues) {
    boolean filterAll = isFilterAll(filterBy);
    boolean filterByCode = "facilityCode".equals(filterBy);
    boolean filterByName = "facilityName".equals(filterBy);

    Collection<MinimalFacilityDto> foundFacilities = new ArrayList<>();

    if ((filterAll || filterByCode || filterByName) && filterValues.isEmpty()) {
      foundFacilities.addAll(facilityReferenceDataService.findAll());
    }

    for (String expression : filterValues) {
      if (filterAll || filterByCode) {
        foundFacilities.addAll(facilityReferenceDataService.search(
            expression, null, null, false));
      }

      if (filterAll || filterByName) {
        foundFacilities.addAll(facilityReferenceDataService.search(
            null, expression, null, false));
      }
    }

    return foundFacilities;
  }

  private boolean isFilterAll(String filterBy) {
    return "all".equals(filterBy);
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
        requisition.getProgramId(),
        period.getId(),
        new PageRequest(Pagination.DEFAULT_PAGE_NUMBER, Pagination.NO_PAGINATION))
        .getContent();
  }

  private void saveStatusMessage(Requisition requisition) {
    if (isNotBlank(requisition.getDraftStatusMessage())) {
      StatusMessage newStatusMessage = StatusMessage.newStatusMessage(requisition,
          authenticationHelper.getCurrentUser().getId(),
          authenticationHelper.getCurrentUser().getFirstName(),
          authenticationHelper.getCurrentUser().getLastName(),
          requisition.getDraftStatusMessage());
      statusMessageRepository.save(newStatusMessage);
      requisition.setDraftStatusMessage("");
    }
  }
}
