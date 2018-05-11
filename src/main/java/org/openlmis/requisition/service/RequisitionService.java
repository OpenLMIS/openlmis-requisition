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
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static java.util.stream.Collectors.toSet;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.APPROVED_QUANTITY;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.APPROVED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_CANNOT_UPDATE_WITH_STATUS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DELETE_FAILED_NEWER_EXISTS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DELETE_FAILED_WRONG_STATUS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_HAVE_SUPPLYING_FACILITY;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PRODUCTS_STOCK_CARDS_MISSING;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_MUST_BE_APPROVED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_MUST_BE_WAITING_FOR_APPROVAL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_CANNOT_CONVERT_WITHOUT_APPROVED_QTY;
import static org.springframework.util.CollectionUtils.isEmpty;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionBuilder;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.StatusChange;
import org.openlmis.requisition.domain.requisition.StatusMessage;
import org.openlmis.requisition.domain.requisition.StockAdjustmentReason;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.DetailedRoleAssignmentDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.IdealStockAmountDto;
import org.openlmis.requisition.dto.MinimalFacilityDto;
import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ProofOfDeliveryDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.SupplyLineDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.dto.stockmanagement.StockCardSummaryDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.openlmis.requisition.service.fulfillment.OrderFulfillmentService;
import org.openlmis.requisition.service.referencedata.ApproveProducts;
import org.openlmis.requisition.service.referencedata.ApprovedProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.IdealStockAmountReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.RightReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserRoleAssignmentsReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.StockCardSummariesStockManagementService;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.utils.Pagination;
import org.openlmis.requisition.utils.RequisitionForConvertComparator;
import org.openlmis.requisition.web.OrderDtoBuilder;
import org.openlmis.requisition.web.RequisitionForConvertBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.profiler.Profiler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Service
// TODO: split this up in OLMIS-1102
@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionService {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionService.class);

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private StatusMessageRepository statusMessageRepository;

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

  @Autowired
  private IdealStockAmountReferenceDataService idealStockAmountReferenceDataService;

  @Autowired
  private StockCardSummariesStockManagementService stockCardSummariesStockManagementService;

  /**
   * Initiated given requisition if possible.
   *
   * @param program Program.
   * @param facility Facility.
   * @param period Period for requisition.
   * @param emergency Emergency status.
   * @param stockAdjustmentReasons list of stockAdjustmentReasons
   * @return Initiated requisition.
   */
  public Requisition initiate(ProgramDto program, FacilityDto facility,
      ProcessingPeriodDto period, boolean emergency,
      List<StockAdjustmentReason> stockAdjustmentReasons,
      RequisitionTemplate requisitionTemplate) {
    Profiler profiler = new Profiler("REQUISITION_INITIATE_SERVICE");
    profiler.setLogger(LOGGER);

    profiler.start("BUILD_REQUISITION");
    Requisition requisition = RequisitionBuilder.newRequisition(
        facility.getId(), program.getId(), emergency);
    requisition.setStatus(RequisitionStatus.INITIATED);

    requisition.setProcessingPeriodId(period.getId());
    requisition.setNumberOfMonthsInPeriod(period.getDurationInMonths());

    profiler.start("GET_PREV_REQUISITIONS_FOR_AVERAGING");
    int numberOfPreviousPeriodsToAverage;
    List<Requisition> previousRequisitions;
    // numberOfPeriodsToAverage is always >= 2 or null
    if (requisitionTemplate.getNumberOfPeriodsToAverage() == null) {
      numberOfPreviousPeriodsToAverage = 0;
      previousRequisitions = getRecentRegularRequisitions(requisition, 1);
    } else {
      numberOfPreviousPeriodsToAverage = requisitionTemplate.getNumberOfPeriodsToAverage() - 1;
      previousRequisitions =
          getRecentRegularRequisitions(requisition, numberOfPreviousPeriodsToAverage);
    }

    if (numberOfPreviousPeriodsToAverage > previousRequisitions.size()) {
      numberOfPreviousPeriodsToAverage = previousRequisitions.size();
    }

    profiler.start("GET_POD");
    ProofOfDeliveryDto pod = null;

    if (!emergency && !isEmpty(previousRequisitions)) {
      pod = proofOfDeliveryService.get(previousRequisitions.get(0));
    }

    profiler.start("FIND_APPROVED_PRODUCTS");
    ApproveProducts approvedProducts = approvedProductReferenceDataService
        .getApprovedProducts(facility.getId(), program.getId());

    profiler.start("FIND_IDEAL_STOCK_AMOUNTS");
    Map<UUID, Integer> idealStockAmounts = idealStockAmountReferenceDataService
        .search(requisition.getFacilityId(), requisition.getProcessingPeriodId())
        .stream()
        .collect(toMap(isa -> isa.getCommodityType().getId(), IdealStockAmountDto::getAmount));

    profiler.start("FIND_STOCK_ON_HANDS");
    Map<UUID, Integer> orderableSoh = getStockOnHands(
        requisitionTemplate, approvedProducts,
        program.getId(), facility.getId(), period.getEndDate()
    );

    profiler.start("INITIATE");
    requisition.initiate(requisitionTemplate, approvedProducts.getFullSupplyProducts(),
        previousRequisitions, numberOfPreviousPeriodsToAverage, pod, idealStockAmounts,
        authenticationHelper.getCurrentUser().getId(), orderableSoh);

    profiler.start("SET_AVAILABLE_PRODUCTS");
    if (emergency) {
      requisition.setAvailableProducts(approvedProducts.getOrderableIds());
    } else {
      requisition.setAvailableProducts(approvedProducts.getNonFullSupplyOrderableIds());
    }

    profiler.start("SET_STOCK_ADJ_REASONS");
    requisition.setStockAdjustmentReasons(stockAdjustmentReasons);

    profiler.start("SAVE");
    requisitionRepository.save(requisition);

    profiler.stop().log();
    return requisition;
  }

  private Map<UUID, Integer> getStockOnHands(RequisitionTemplate requisitionTemplate,
                                             ApproveProducts approveProducts,
                                             UUID programId, UUID facilityId, LocalDate endDate) {
    if (requisitionTemplate.isPopulateStockOnHandFromStockCards()) {
      List<StockCardSummaryDto> cards = stockCardSummariesStockManagementService.search(
          programId,
          facilityId,
          approveProducts.getFullSupplyOrderableIds(),
          endDate);

      validateNoSohIsMissing(approveProducts, cards);

      return cards
          .stream()
          .collect(Collectors.toMap(
              card -> card.getOrderable().getId(),
              StockCardSummaryDto::getStockOnHand
          ));
    } else {
      return Collections.emptyMap();
    }
  }

  private void validateNoSohIsMissing(ApproveProducts approveProducts,
                                      List<StockCardSummaryDto> cards) {
    List<StockCardSummaryDto> cardsWithNullSoh = cards.stream()
        .filter(c -> c.getStockOnHand() == null)
        .collect(Collectors.toList());

    if (!cardsWithNullSoh.isEmpty()) {
      OrderableDto orderableWithNoSoh =
          getFirstOrderableWithNullSoh(approveProducts, cardsWithNullSoh);

      throw new ValidationMessageException(ERROR_PRODUCTS_STOCK_CARDS_MISSING,
          orderableWithNoSoh.getFullProductName(), cardsWithNullSoh.size());
    }
  }

  private OrderableDto getFirstOrderableWithNullSoh(
      ApproveProducts approveProducts,
      List<StockCardSummaryDto> cardsWithNullSoh) {
    return approveProducts
        .getFullSupplyProduct(cardsWithNullSoh.get(0).getOrderable().getId())
        .getOrderable();
  }

  /**
   * Delete given Requisition if possible.
   *
   * @param requisition Requisition to be deleted.
   */
  public void delete(Requisition requisition) {
    if (!requisition.isDeletable()) {
      throw new ValidationMessageException(ERROR_DELETE_FAILED_WRONG_STATUS);
    } else if (!requisition.getEmergency() && !isRequisitionNewest(requisition)) {
      throw new ValidationMessageException(ERROR_DELETE_FAILED_NEWER_EXISTS);
    } else {
      statusMessageRepository
          .delete(statusMessageRepository.findByRequisitionId(requisition.getId()));
      requisitionRepository.delete(requisition);
      LOGGER.debug("Requisition deleted");
    }
  }

  /**
   * Reject given requisition if possible.
   *
   * @param requisition Requisition to be rejected.
   */
  public Requisition reject(Requisition requisition, Map<UUID, OrderableDto> orderables) {
    if (requisition.isApprovable()) {
      UserDto currentUser = authenticationHelper.getCurrentUser();
      UUID userId = currentUser.getId();
      validateCanApproveRequisition(requisition, userId).throwExceptionIfHasErrors();

      LOGGER.debug("Requisition rejected: {}", requisition.getId());
      requisition.reject(orderables, userId);
      saveStatusMessage(requisition, currentUser);
      return requisitionRepository.save(requisition);
    } else {
      throw new ValidationMessageException(new Message(
          ERROR_REQUISITION_MUST_BE_WAITING_FOR_APPROVAL, requisition.getId()));
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
  public Page<Requisition> searchRequisitions(Set<RequisitionStatus> requisitionStatuses,
                                              Pageable pageable) {
    return requisitionRepository.searchRequisitions(null, null, null, null, null,
        null, requisitionStatuses, null, permissionService.getPermissionStrings(), pageable);
  }

  /**
   * Get requisitions to approve for the specified user.
   */
  public Page<Requisition> getRequisitionsForApproval(UUID userId, UUID programId,
      Pageable pageable) {
    Profiler profiler = new Profiler("REQUISITION_SERVICE_GET_FOR_APPROVAL");
    profiler.setLogger(LOGGER);

    Page<Requisition> requisitionsForApproval = Pagination.getPage(
        Collections.emptyList(), pageable);

    profiler.start("FIND_RIGHT_FROM_NAME");
    RightDto right = rightReferenceDataService.findRight(PermissionService.REQUISITION_APPROVE);

    profiler.start("GET_USER_DETAILED_ROLE_ASSIGNMENTS");
    List<DetailedRoleAssignmentDto> roleAssignments = userRoleAssignmentsReferenceDataService
        .getRoleAssignments(userId)
        .stream()
        .filter(r -> r.getRole().getRights().contains(right))
        .collect(toList());

    if (roleAssignments != null && !roleAssignments.isEmpty()) {
      profiler.start("GET_PROGRAM_AND_NODE_IDS_FROM_ROLE_ASSIGNMENTS");
      Set<Pair> programNodePairs = new HashSet<>();
      for (DetailedRoleAssignmentDto roleAssignment : roleAssignments) {
        if (roleAssignment.getSupervisoryNodeId() != null
            && roleAssignment.getProgramId() != null
                && (programId == null || programId.equals(roleAssignment.getProgramId()))) {
          programNodePairs.add(new ImmutablePair<>(
              roleAssignment.getProgramId(), roleAssignment.getSupervisoryNodeId()));
        }
      }
      profiler.start("REQUISITION_REPOSITORY_SEARCH_APPROVABLE_BY_PAIRS");
      requisitionsForApproval = requisitionRepository
          .searchApprovableRequisitionsByProgramSupervisoryNodePairs(programNodePairs, pageable);
    }

    profiler.stop().log();
    return requisitionsForApproval;
  }

  /**
   * Performs several validation checks to ensure that the given requisition can be approved.
   * It makes sure that the user has got rights to approve the requisition, that the requisition
   * exists and that it has got correct status to be eligible for approval.
   *
   * @param requisition the requisition to verify
   * @param userId the UUID of the user approving the requisition
   * @return ValidationResult instance containing the outcome of this validation
   */
  public ValidationResult validateCanApproveRequisition(Requisition requisition, UUID userId) {

    ValidationResult permissionCheck = permissionService.canApproveRequisition(requisition);
    if (permissionCheck.hasErrors()) {
      return permissionCheck;
    }

    if (!requisition.isApprovable()) {
      return ValidationResult.failedValidation(MessageKeys
          .ERROR_REQUISITION_MUST_BE_AUTHORIZED, requisition.getId());
    }

    RightDto right = rightReferenceDataService.findRight(PermissionService.REQUISITION_APPROVE);
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
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (isNull(requisition)) {
      return ValidationResult.notFound(ERROR_REQUISITION_NOT_FOUND, requisitionId);
    }

    return validateCanSaveRequisition(requisition);
  }

  /**
   * Performs several validation checks to ensure that the given requisition can be saved.
   * It makes sure that the user has got rights to save the requisition, that the requisition
   * exists and that it has got correct status to be eligible for saving.
   *
   * @param requisition the requisition for which the request was made
   * @return ValidationResult instance containing the outcome of this validation
   */
  public ValidationResult validateCanSaveRequisition(Requisition requisition) {
    ValidationResult permissionCheck = permissionService.canUpdateRequisition(requisition);
    if (permissionCheck.hasErrors()) {
      return permissionCheck;
    }

    RequisitionStatus status = requisition.getStatus();
    if (!status.isUpdatable()) {
      return ValidationResult.failedValidation(ERROR_CANNOT_UPDATE_WITH_STATUS, status);
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
    Profiler profiler = new Profiler("RELEASE_REQUISITIONS_AS_ORDER");
    profiler.setLogger(LOGGER);

    profiler.start("GET_ORDERS_EDIT_RIGHT_DTO");
    RightDto right = authenticationHelper.getRight(PermissionService.ORDERS_EDIT);
    List<Requisition> releasedRequisitions = new ArrayList<>();

    profiler.start("GET_USER_FULFILLMENT_FACILITIES");
    Set<UUID> userFacilities = fulfillmentFacilitiesReferenceDataService
        .getFulfillmentFacilities(user.getId(), right.getId()).stream().map(FacilityDto::getId)
        .collect(toSet());

    profiler.start("RELEASE");
    for (ConvertToOrderDto convertToOrderDto : convertToOrderDtos) {
      UUID requisitionId = convertToOrderDto.getRequisitionId();
      Requisition loadedRequisition = requisitionRepository.findOne(requisitionId);
      isEligibleForConvertToOrder(loadedRequisition).throwExceptionIfHasErrors();
      loadedRequisition.release(authenticationHelper.getCurrentUser().getId());

      UUID facilityId = convertToOrderDto.getSupplyingDepotId();
      Set<UUID> validFacilities = requisitionForConvertBuilder
          .getAvailableSupplyingDepots(requisitionId).stream()
          .filter(f -> userFacilities.contains(f.getId())).map(FacilityDto::getId)
          .collect(toSet());

      if (validFacilities.contains(facilityId)) {
        loadedRequisition.setSupplyingFacilityId(facilityId);
      } else {
        throw new ValidationMessageException(new Message(ERROR_MUST_HAVE_SUPPLYING_FACILITY,
            loadedRequisition.getId()));
      }

      releasedRequisitions.add(loadedRequisition);
    }

    profiler.stop().log();
    return releasedRequisitions;
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
    Profiler profiler = new Profiler("SEARCH_APPROVED_REQUISITIONS_SERVICE");
    profiler.setLogger(LOGGER);

    profiler.start("FIND_DESIRED_PROGRAMS");
    Map<UUID, ProgramDto> programs = findProgramsWithFilter(filterBy, filterValues);

    profiler.start("FIND_DESIRED_FACILITIES");
    Map<UUID, MinimalFacilityDto> facilities = findFacilitiesWithFilter(filterBy, filterValues);

    profiler.start("SEARCH_APPROVED_REQUISITIONS");
    List<Requisition> requisitionsList =
        requisitionRepository.searchApprovedRequisitions(filterBy,
            facilities.keySet(),
            programs.keySet());

    profiler.start("BUILD_DTOS");
    List<RequisitionWithSupplyingDepotsDto> responseList =
        requisitionForConvertBuilder.buildRequisitions(requisitionsList, userManagedFacilities,
            facilities, programs);

    profiler.start("SORT");
    responseList.sort(new RequisitionForConvertComparator(pageable));

    profiler.start("PAGINATE");
    Page<RequisitionWithSupplyingDepotsDto> page = Pagination.getPage(responseList, pageable);

    profiler.stop().log();
    return page;
  }

  /**
   * Converting Requisition list to Orders.
   */
  public void convertToOrder(List<ConvertToOrderDto> list, UserDto user) {
    Profiler profiler = new Profiler("CONVERT_TO_ORDER");
    profiler.setLogger(LOGGER);

    profiler.start("RELEASE_REQUISITIONS_AS_ORDER");
    List<Requisition> releasedRequisitions = releaseRequisitionsAsOrder(list, user);

    profiler.start("BUILD_ORDER_DTOS_AND_SAVE_REQUISITION");
    List<OrderDto> orders = new ArrayList<>();
    for (Requisition requisition : releasedRequisitions) {
      OrderDto order = orderDtoBuilder.build(requisition, user);
      orders.add(order);

      requisitionRepository.save(requisition);
      requisitionStatusProcessor.statusChange(requisition);
    }

    profiler.start("CREATE_ORDER_IN_FULFILLMENT");
    orderFulfillmentService.create(orders);

    profiler.stop().log();
  }

  /**
   * Saves status message of a requisition if its draft is not empty.
   */
  public void saveStatusMessage(Requisition requisition, UserDto currentUser) {
    if (isNotBlank(requisition.getDraftStatusMessage())) {
      // find the status change we are about to add. If it's already persisted,
      // get the latest one by date created
      StatusChange statusChange = requisition.getStatusChanges().stream()
          .filter(sc -> sc.getId() == null)
          .findFirst()
          .orElse(requisition.getLatestStatusChange());
      StatusMessage newStatusMessage = StatusMessage.newStatusMessage(requisition,
          statusChange,
          currentUser.getId(),
          currentUser.getFirstName(),
          currentUser.getLastName(),
          requisition.getDraftStatusMessage());
      statusMessageRepository.save(newStatusMessage);
      requisition.setDraftStatusMessage("");
    }
  }

  /**
   * Approves requisition.
   *
   * @param parentNodeId supervisoryNode that has a supply line for the requisition's program.
   * @param currentUser user who approves this requisition.
   * @param orderables orderable products that will be used by line items to update packs to ship.
   * @param requisition requisition to be approved
   * @param supplyLines supplyLineDtos of the supervisoryNode that has a supply line for the
   *                    requisition's program.
   */
  public void doApprove(UUID parentNodeId, UserDto currentUser, Map<UUID, OrderableDto> orderables,
      Requisition requisition, List<SupplyLineDto> supplyLines) {
    requisition.approve(parentNodeId, orderables, supplyLines, currentUser.getId());

    saveStatusMessage(requisition, currentUser);
    requisitionRepository.saveAndFlush(requisition);
  }

  private boolean isRequisitionNewest(Requisition requisition) {
    Requisition recentRequisition = findRecentRegularRequisition(
        requisition.getProgramId(), requisition.getFacilityId()
    );
    return null == recentRequisition || requisition.getId().equals(recentRequisition.getId());
  }

  /**
   * Returns requisition associated with the most recent period for given program and facility.
   *
   * @param programId  Program for Requisition
   * @param facilityId Facility for Requisition
   * @return Requisition.
   */
  private Requisition findRecentRegularRequisition(UUID programId, UUID facilityId) {
    Requisition result = null;
    Collection<ProcessingPeriodDto> periods =
        periodService.searchByProgramAndFacility(programId, facilityId);

    if (periods != null) {
      for (ProcessingPeriodDto dto : periods) {
        // There is always maximum one regular requisition for given period, facility and program
        List<Requisition> requisitions = requisitionRepository.searchRequisitions(
            dto.getId(), facilityId, programId, false);

        if (!requisitions.isEmpty()) {
          result = requisitions.get(0);
        } else {
          break;
        }
      }
    }

    return result;
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

  private Map<UUID, ProgramDto> findProgramsWithFilter(String filterBy,
                                                       List<String> filterValues) {
    Collection<ProgramDto> foundPrograms = new HashSet<>();

    if (CollectionUtils.isEmpty(filterValues)
        || !isFilterByProgramProperty(filterBy)) {
      foundPrograms = programReferenceDataService.findAll();
    } else {
      for (String expression : filterValues) {
        foundPrograms.addAll(programReferenceDataService.search(expression));
      }
    }

    return foundPrograms.stream().collect(toMap(ProgramDto::getId, Function.identity()));
  }

  private Map<UUID, MinimalFacilityDto> findFacilitiesWithFilter(String filterBy,
                                                                  List<String> filterValues) {
    Collection<MinimalFacilityDto> foundFacilities = new HashSet<>();

    if (CollectionUtils.isEmpty(filterValues)
        || !isFilterByFacilityProperty(filterBy)) {
      foundFacilities.addAll(facilityReferenceDataService.findAll());
    } else {
      for (String expression : filterValues) {
        String code = isFilterAll(filterBy) || "facilityCode".equals(filterBy) ? expression : null;
        String name = isFilterAll(filterBy) || "facilityName".equals(filterBy) ? expression : null;

        foundFacilities.addAll(facilityReferenceDataService.search(code, name, null, false));
      }
    }

    return foundFacilities.stream()
        .collect(toMap(MinimalFacilityDto::getId, Function.identity()));
  }

  private boolean isFilterAll(String filterBy) {
    return "all".equalsIgnoreCase(filterBy);
  }

  private boolean isFilterByFacilityProperty(String filterBy) {
    return "facilityCode".equalsIgnoreCase(filterBy)
        || "facilityName".equalsIgnoreCase(filterBy)
        || isFilterAll(filterBy);
  }

  private boolean isFilterByProgramProperty(String filterBy) {
    return isFilterAll(filterBy) || "programName".equalsIgnoreCase(filterBy);
  }

  private List<Requisition> getRecentRegularRequisitions(Requisition requisition, int amount) {
    List<ProcessingPeriodDto> previousPeriods =
        periodService.findPreviousPeriods(requisition.getProcessingPeriodId(), amount);

    List<Requisition> recentRequisitions = new ArrayList<>();
    for (ProcessingPeriodDto period : previousPeriods) {
      List<Requisition> requisitionsByPeriod = getRegularRequisitionsByPeriod(requisition, period);
      if (!requisitionsByPeriod.isEmpty()) {
        Requisition requisitionByPeriod = requisitionsByPeriod.get(0);
        recentRequisitions.add(requisitionByPeriod);
      }
    }
    return recentRequisitions;
  }

  private List<Requisition> getRegularRequisitionsByPeriod(Requisition requisition,
                                                           ProcessingPeriodDto period) {
    return requisitionRepository.searchRequisitions(
        period.getId(), requisition.getFacilityId(), requisition.getProgramId(), false);
  }
}
