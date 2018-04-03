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

package org.openlmis.requisition.web;

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_ID_MISMATCH;
import static org.springframework.web.bind.annotation.RequestMethod.GET;
import static org.springframework.web.bind.annotation.RequestMethod.POST;

import java.time.LocalDate;
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionBuilder;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.StockAdjustmentReason;
import org.openlmis.requisition.dto.BasicRequisitionDto;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ReasonDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.SupplyLineDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.dto.ValidReasonDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionStatusNotifier;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.ValidReasonStockmanagementService;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.utils.Pagination;
import org.openlmis.requisition.validate.ReasonsValidator;
import org.slf4j.profiler.Profiler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Controller;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

@SuppressWarnings("PMD.TooManyMethods")
@Controller
@Transactional
public class RequisitionController extends BaseRequisitionController {
  private static final String BUILD_DTO_LIST = "BUILD_DTO_LIST";

  @Autowired
  private PeriodService periodService;

  @Autowired
  private UserFulfillmentFacilitiesReferenceDataService fulfillmentFacilitiesReferenceDataService;

  @Autowired
  private RequisitionStatusNotifier requisitionStatusNotifier;

  @Autowired
  private ValidReasonStockmanagementService validReasonStockmanagementService;

  @Autowired
  private SupervisoryNodeReferenceDataService supervisoryNodeService;

  @Autowired
  private ReasonsValidator reasonsValidator;

  /**
   * Allows creating new requisitions.
   *
   * @param programId         UUID of Program.
   * @param facilityId        UUID of Facility.
   * @param emergency       Emergency status.
   * @param suggestedPeriod Period for requisition.
   * @return created requisition.
   */
  @RequestMapping(value = "/requisitions/initiate", method = POST)
  @ResponseStatus(HttpStatus.CREATED)
  @ResponseBody
  public RequisitionDto initiate(@RequestParam(value = "program") UUID programId,
                    @RequestParam(value = "facility") UUID facilityId,
                    @RequestParam(value = "suggestedPeriod", required = false) UUID suggestedPeriod,
                    @RequestParam(value = "emergency") boolean emergency) {
    Profiler profiler = getProfiler(
        "POST_REQUISITION_INITIATE",
        programId, facilityId, suggestedPeriod, emergency
    );

    if (null == facilityId || null == programId) {
      throw new ValidationMessageException(
          new Message(MessageKeys.ERROR_INITIALIZE_MISSING_PARAMETERS));
    }

    checkPermission(profiler, () -> permissionService.canInitRequisition(programId, facilityId));
    FacilityDto facility = findFacility(facilityId, profiler);

    profiler.start("CHECK_FACILITY_SUPPORTS_PROGRAM");
    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facility, programId);

    profiler.start("GET_STOCK_ADJ_REASONS");
    List<StockAdjustmentReason> stockAdjustmentReasons =
        getStockAdjustmentReasons(programId, facility);

    ProgramDto program = findProgram(programId, profiler);

    profiler.start("INITIATE_REQUISITION");
    Requisition newRequisition = requisitionService.initiate(
        program, facility, suggestedPeriod, emergency, stockAdjustmentReasons);

    profiler.start("VALIDATE_REASONS");
    reasonsValidator.validate(stockAdjustmentReasons, newRequisition.getTemplate());

    RequisitionDto requisitionDto = buildDto(
        profiler, newRequisition,
        findOrderables(profiler, newRequisition::getAllOrderableIds),
        facility, program
    );
    stopProfiler(profiler, requisitionDto);

    return requisitionDto;
  }

  /**
   * Returns processing periods for unprocessed requisitions.
   *
   * @param programId   UUID of the Program.
   * @param facilityId  UUID of the Facility.
   * @param emergency true for periods to initiate an emergency requisition; false otherwise.
   * @return processing periods.
   */
  @RequestMapping(value = "/requisitions/periodsForInitiate", method = GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public Collection<ProcessingPeriodDto> getProcessingPeriodIds(
      @RequestParam(value = "programId") UUID programId,
      @RequestParam(value = "facilityId") UUID facilityId,
      @RequestParam(value = "emergency") boolean emergency) {
    Profiler profiler = getProfiler("GET_PERIODS_FOR_INITIATE_REQUISITION", programId,
        facilityId, emergency);

    if (null == facilityId || null == programId) {
      throw new ValidationMessageException(
          new Message(MessageKeys.ERROR_REQUISITION_PERIODS_FOR_INITIATE_MISSING_PARAMETERS));
    }

    checkPermission(
        profiler,
        () -> permissionService.canInitOrAuthorizeRequisition(programId, facilityId)
    );

    profiler.start("CHECK_IF_FACILITY_SUPPORTS_PROGRAM");
    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facilityId, programId);

    profiler.start("GET_PERIODS");
    Collection<ProcessingPeriodDto> periods = periodService.getPeriods(
        programId, facilityId, emergency
    );

    stopProfiler(profiler, periods);

    return periods;
  }

  /**
   * Submits earlier initiated requisition.
   */
  @RequestMapping(value = "/requisitions/{id}/submit", method = RequestMethod.POST)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public BasicRequisitionDto submitRequisition(@PathVariable("id") UUID requisitionId) {
    Profiler profiler = getProfiler("SUBMIT_REQUISITION", requisitionId);
    Requisition requisition = findRequisition(requisitionId, profiler);
    checkPermission(profiler, () -> permissionService.canSubmitRequisition(requisition));
    validateForStatusChange(requisition, profiler);
    checkIfPeriodIsValid(requisition, profiler);

    logger.debug("Submitting a requisition with id " + requisition.getId());

    ProgramDto program = findProgram(requisition.getProgramId(), profiler);
    Map<UUID, OrderableDto> orderables = findOrderables(
        profiler, () -> getLineItemOrderableIds(requisition)
    );

    profiler.start("SUBMIT");
    requisition.submit(orderables, getCurrentUser(profiler).getId(),
        program.getSkipAuthorization());

    profiler.start("SAVE");
    requisitionService.saveStatusMessage(requisition, authenticationHelper.getCurrentUser());
    requisitionRepository.save(requisition);

    callStatusChangeProcessor(profiler, requisition);
    logger.debug("Requisition with id " + requisition.getId() + " submitted");

    BasicRequisitionDto dto = buildBasicDto(profiler, requisition);

    stopProfiler(profiler, dto);

    return dto;
  }

  /**
   * Deletes requisition with the given id.
   */
  @RequestMapping(value = "/requisitions/{id}", method = RequestMethod.DELETE)
  @ResponseStatus(HttpStatus.NO_CONTENT)
  public void deleteRequisition(@PathVariable("id") UUID requisitionId) {
    Profiler profiler = getProfiler("DELETE_REQUISITION", requisitionId);
    Requisition requisition = findRequisition(requisitionId, profiler);
    checkPermission(profiler, () -> permissionService.canDeleteRequisition(requisition));

    profiler.start("DELETE");
    requisitionService.delete(requisition);

    stopProfiler(profiler);
  }

  /**
   * Allows updating requisitions.
   *
   * @param requisitionDto A requisitionDto bound to the request body.
   * @param requisitionId  UUID of requisition which we want to update.
   * @return updated requisition.
   */
  @RequestMapping(value = "/requisitions/{id}", method = RequestMethod.PUT)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public RequisitionDto updateRequisition(@RequestBody RequisitionDto requisitionDto,
                                          @PathVariable("id") UUID requisitionId) {
    Profiler profiler = getProfiler("UPDATE_REQUISITION", requisitionId, requisitionDto);

    if (null != requisitionDto.getId() && !Objects.equals(requisitionDto.getId(), requisitionId)) {
      throw new ValidationMessageException(ERROR_ID_MISMATCH);
    }

    Requisition requisitionToUpdate = findRequisition(requisitionId, profiler);
    checkPermission(
        profiler,
        () -> requisitionService.validateCanSaveRequisition(requisitionToUpdate)
    );

    Map<UUID, OrderableDto> orderables = findOrderables(
        profiler, requisitionToUpdate::getAllOrderableIds
    );

    profiler.start("BUILD_REQUISITION_UPDATER");
    Requisition requisition = RequisitionBuilder.newRequisition(requisitionDto,
        requisitionToUpdate.getTemplate(), requisitionToUpdate.getProgramId(),
        requisitionToUpdate.getStatus(), orderables);
    requisition.setId(requisitionId);

    profiler.start("VALIDATE_TIMESTAMPS");
    requisitionVersionValidator.validateRequisitionTimestamps(requisition, requisitionToUpdate)
        .throwExceptionIfHasErrors();

    ProgramDto program = findProgram(requisitionToUpdate.getProgramId(), profiler);

    profiler.start("VALIDATE_CAN_BE_UPDATED");
    validateRequisitionCanBeUpdated(requisitionToUpdate, requisition, program)
        .throwExceptionIfHasErrors();

    logger.debug("Updating requisition with id: {}", requisitionId);

    FacilityDto facility = findFacility(requisitionToUpdate.getFacilityId(), profiler);

    RequisitionDto dto = doUpdate(
        requisitionToUpdate, requisition, orderables, facility, program, profiler
    );

    stopProfiler(profiler, dto);
    return dto;
  }

  /**
   * Get chosen requisition.
   *
   * @param requisitionId UUID of requisition whose we want to get
   * @return Requisition.
   */
  @RequestMapping(value = "/requisitions/{id}", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public RequisitionDto getRequisition(@PathVariable("id") UUID requisitionId) {
    Profiler profiler = getProfiler("GET_REQUISITION", requisitionId);
    Requisition requisition = findRequisition(requisitionId, profiler);
    checkPermission(profiler, () -> permissionService.canViewRequisition(requisition));
    RequisitionDto requisitionDto = buildDto(
        profiler, requisition,
        findOrderables(profiler, requisition::getAllOrderableIds),
        findFacility(requisition.getFacilityId(), profiler),
        findProgram(requisition.getProgramId(), profiler)
    );

    stopProfiler(profiler, requisitionDto);
    return requisitionDto;
  }

  /**
   * Finds requisitions matching all of the provided parameters.
   */
  @RequestMapping(value = "/requisitions/search", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public Page<BasicRequisitionDto> searchRequisitions(
      @RequestParam(value = "facility", required = false) UUID facility,
      @RequestParam(value = "program", required = false) UUID program,
      @RequestParam(value = "initiatedDateFrom", required = false)
      @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate initiatedDateFrom,
      @RequestParam(value = "initiatedDateTo", required = false)
      @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate initiatedDateTo,
      @RequestParam(value = "processingPeriod", required = false)
          UUID processingPeriod,
      @RequestParam(value = "supervisoryNode", required = false) UUID supervisoryNode,
      @RequestParam(value = "requisitionStatus", required = false)
          Set<RequisitionStatus> requisitionStatuses,
      @RequestParam(value = "emergency", required = false) Boolean emergency,
      Pageable pageable) {
    Profiler profiler = getProfiler(
        "REQUISITIONS_SEARCH",
        facility, program, initiatedDateFrom, initiatedDateTo, processingPeriod, supervisoryNode,
        requisitionStatuses, pageable
    );

    profiler.start("REQUISITION_SERVICE_SEARCH");
    Page<Requisition> requisitionPage = requisitionService.searchRequisitions(facility, program,
        initiatedDateFrom, initiatedDateTo, processingPeriod, supervisoryNode, requisitionStatuses,
        emergency, pageable);

    profiler.start("REQUISITION_DTO_BUILD");
    assert requisitionPage != null;
    Page<BasicRequisitionDto> requisitionDtoPage = Pagination.getPage(
        basicRequisitionDtoBuilder.build(requisitionPage.getContent()),
        pageable,
        requisitionPage.getTotalElements());

    stopProfiler(profiler, requisitionDtoPage);
    return requisitionDtoPage;
  }

  /**
   * Skipping chosen requisition period.
   */
  @RequestMapping(value = "/requisitions/{id}/skip", method = RequestMethod.PUT)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public BasicRequisitionDto skipRequisition(@PathVariable("id") UUID requisitionId) {
    Profiler profiler = getProfiler("SKIP_REQUISITION", requisitionId);
    Requisition requisition = findRequisition(requisitionId, profiler);
    checkPermission(profiler, () -> permissionService.canUpdateRequisition(requisition));

    profiler.start("SKIP");
    Requisition skippedRequisition = requisitionService.skip(requisition);

    callStatusChangeProcessor(profiler, skippedRequisition);

    BasicRequisitionDto dto = buildBasicDto(profiler, skippedRequisition);

    stopProfiler(profiler, dto);
    return dto;
  }

  /**
   * Rejecting requisition which is waiting for approve.
   */
  @RequestMapping(value = "/requisitions/{id}/reject", method = RequestMethod.PUT)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public BasicRequisitionDto rejectRequisition(@PathVariable("id") UUID requisitionId) {
    Profiler profiler = getProfiler("REJECT", requisitionId);
    Requisition requisition = findRequisition(requisitionId, profiler);
    checkPermission(profiler, () -> permissionService.canApproveRequisition(requisition));
    Map<UUID, OrderableDto> orderables = findOrderables(
        profiler, () -> getLineItemOrderableIds(requisition)
    );

    profiler.start("REJECT");
    Requisition rejectedRequisition = requisitionService.reject(requisition, orderables);

    callStatusChangeProcessor(profiler, rejectedRequisition);

    profiler.start("NOTIFY_STATUS_CHANGED");
    requisitionStatusNotifier.notifyStatusChanged(rejectedRequisition);

    BasicRequisitionDto dto = buildBasicDto(profiler, rejectedRequisition);

    stopProfiler(profiler, dto);
    return dto;
  }

  /**
   * Approve specified by id requisition.
   */
  @RequestMapping(value = "/requisitions/{id}/approve", method = RequestMethod.POST)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public BasicRequisitionDto approveRequisition(@PathVariable("id") UUID requisitionId) {
    Profiler profiler = getProfiler("APPROVE_REQUISITION", requisitionId);
    Requisition requisition = findRequisition(requisitionId, profiler);
    UserDto user = getCurrentUser(profiler);
    checkPermission(
        profiler,
        () -> requisitionService.validateCanApproveRequisition(requisition, user.getId())
    );

    validateForStatusChange(requisition, profiler);

    SupervisoryNodeDto supervisoryNodeDto = getSupervisoryNodeDto(profiler, requisition);
    Map<UUID, OrderableDto> orderables = findOrderables(profiler, requisition);
    List<SupplyLineDto> supplyLines = getSupplyLineDtos(profiler, requisition);

    profiler.start("DO_APPROVE");
    ApproveParams approveParams = new ApproveParams(user, supervisoryNodeDto, orderables,
        supplyLines);
    doApprove(requisition, approveParams);

    BasicRequisitionDto requisitionDto = buildBasicDto(profiler, requisition);

    submitStockEvent(requisition, user.getId());

    stopProfiler(profiler, requisitionDto);
    return requisitionDto;
  }

  /**
   * Get requisitions to approve for right supervisor.
   *
   * @return Approved requisitions.
   */
  @RequestMapping(value = "/requisitions/requisitionsForApproval", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public Page<BasicRequisitionDto> requisitionsForApproval(
          @RequestParam(value = "program", required = false) UUID programId,
          Pageable pageable) {
    Profiler profiler = getProfiler("REQUISITIONS_FOR_APPROVAL", programId, pageable);
    UserDto user = getCurrentUser(profiler);

    profiler.start("REQUISITION_SERVICE_GET_FOR_APPROVAL");
    Page<Requisition> approvalRequisitions = requisitionService
        .getRequisitionsForApproval(user.getId(), programId, pageable);

    profiler.start(BUILD_DTO_LIST);
    Page<BasicRequisitionDto> dtoPage = Pagination.getPage(
        basicRequisitionDtoBuilder.build(approvalRequisitions.getContent()),
        pageable,
        approvalRequisitions.getTotalElements());

    stopProfiler(profiler);
    return dtoPage;
  }

  /**
   * Get all submitted Requisitions.
   *
   * @return Submitted requisitions.
   */
  @RequestMapping(value = "/requisitions/submitted", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public Page<RequisitionDto> getSubmittedRequisitions(Pageable pageable) {
    Profiler profiler = getProfiler("GET_SUBMITTED_REQUISITIONS", pageable);

    profiler.start("SEARCH_REQUISITIONS");
    Page<Requisition> submittedRequisitions = requisitionService.searchRequisitions(
        EnumSet.of(RequisitionStatus.SUBMITTED), pageable);

    profiler.start(BUILD_DTO_LIST);
    Page<RequisitionDto> page = Pagination.getPage(
        requisitionDtoBuilder.build(submittedRequisitions.getContent()),
        pageable,
        submittedRequisitions.getTotalElements());

    stopProfiler(profiler);
    return page;
  }

  /**
   * Authorize given requisition.
   *
   * @param requisitionId UUID of Requisition to authorize.
   * @return authorized Requisition.
   */
  @RequestMapping(value = "/requisitions/{id}/authorize", method = RequestMethod.POST)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public BasicRequisitionDto authorizeRequisition(@PathVariable("id") UUID requisitionId) {
    Profiler profiler = getProfiler("AUTHORIZE_REQUISITION", requisitionId);
    Requisition requisition = findRequisition(requisitionId, profiler);
    checkPermission(profiler, () -> permissionService.canAuthorizeRequisition(requisition));
    validateForStatusChange(requisition, profiler);
    checkIfPeriodIsValid(requisition, profiler);

    UserDto user = getCurrentUser(profiler);

    Map<UUID, OrderableDto> orderables = findOrderables(
        profiler, () -> getLineItemOrderableIds(requisition)
    );

    profiler.start("AUTHORIZE");
    requisition.authorize(orderables, user.getId());

    profiler.start("SAVE");
    requisitionService.saveStatusMessage(requisition, user);
    requisitionRepository.save(requisition);

    callStatusChangeProcessor(profiler, requisition);
    logger.debug("Requisition: " + requisitionId + " authorized.");

    BasicRequisitionDto dto = buildBasicDto(profiler, requisition);

    stopProfiler(profiler, dto);
    return dto;
  }

  /**
   * Get approved requisitions matching all of provided parameters.
   *
   * @param filterValue  List of expressions to be used in filters.
   * @param filterBy     Field used to filter: "programName", "facilityCode", "facilityName" or
   *                     "all".
   * @param pageable     Pageable object that allows client to optionally add "page" (page number)
   *                     and "size" (page size) query parameters to the request.
   * @return Page of approved requisitions.
   */
  @RequestMapping(value = "/requisitions/requisitionsForConvert", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public Page<RequisitionWithSupplyingDepotsDto> listForConvertToOrder(
      @RequestParam(required = false) List<String> filterValue,
      @RequestParam(required = false) String filterBy,
      Pageable pageable) {
    Profiler profiler = getProfiler(
        "GET_REQUISITIONS_FOR_CONVERT",
        filterBy, filterValue, pageable
    );

    UserDto user = getCurrentUser(profiler);

    profiler.start("GET_RIGHT");
    RightDto right = authenticationHelper.getRight(PermissionService.ORDERS_EDIT);

    profiler.start("GET_USER_MANAGED_FACILITIES");
    Collection<UUID> userManagedFacilities = fulfillmentFacilitiesReferenceDataService
        .getFulfillmentFacilities(user.getId(), right.getId())
        .stream().map(FacilityDto::getId).collect(Collectors.toList());

    profiler.start("SEARCH_FOR_APPROVED_REQUISITIONS");
    Page<RequisitionWithSupplyingDepotsDto> page = requisitionService
        .searchApprovedRequisitionsWithSortAndFilterAndPaging(
            filterValue,
            filterBy,
            pageable,
            userManagedFacilities);

    stopProfiler(profiler, page);
    return page;
  }

  /**
   * Converting Requisition list to orders.
   *
   * @param list List of Requisitions with their supplyingDepots that will be converted to Orders
   */
  @RequestMapping(value = "/requisitions/convertToOrder", method = RequestMethod.POST)
  @ResponseStatus(HttpStatus.CREATED)
  public void convertToOrder(@RequestBody List<ConvertToOrderDto> list) {
    Profiler profiler = getProfiler("CONVERT_TO_ORDER", list);
    checkPermission(profiler, () -> permissionService.canConvertToOrder(list));

    profiler.start("CONVERT");
    requisitionService.convertToOrder(list, getCurrentUser(profiler));

    stopProfiler(profiler);
  }

  private SupervisoryNodeDto getSupervisoryNodeDto(Profiler profiler, Requisition requisition) {
    profiler.start("GET_SUPERVISORY_NODE");
    return supervisoryNodeService
        .findOne(requisition.getSupervisoryNodeId());
  }

  private Map<UUID, OrderableDto> findOrderables(Profiler profiler, Requisition requisition) {
    return findOrderables(
        profiler, () -> getLineItemOrderableIds(requisition)
    );
  }

  private List<SupplyLineDto> getSupplyLineDtos(Profiler profiler, Requisition requisition) {
    profiler.start("GET_SUPPLY_LINE");
    return supplyLineReferenceDataService.search(
        requisition.getProgramId(), requisition.getSupervisoryNodeId());
  }

  private List<StockAdjustmentReason> getStockAdjustmentReasons(UUID programId,
                                                                FacilityDto facilityDto) {
    List<ValidReasonDto> validReasons =
        validReasonStockmanagementService
            .search(programId, facilityDto.getType().getId());

    List<ReasonDto> reasonDtos = validReasons.stream()
        .map(ValidReasonDto::getReasonWithHidden)
        .collect(Collectors.toList());

    return StockAdjustmentReason.newInstance(reasonDtos);
  }

}
