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

import static java.util.Objects.isNull;
import static org.apache.commons.lang3.BooleanUtils.isNotTrue;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_ID_MISMATCH;
import static org.springframework.web.bind.annotation.RequestMethod.GET;
import static org.springframework.web.bind.annotation.RequestMethod.POST;

import java.time.LocalDate;
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
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
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.dto.ValidReasonDto;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.RequisitionStatusNotifier;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.ValidReasonStockmanagementService;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.utils.Pagination;
import org.openlmis.requisition.utils.RightName;
import org.openlmis.requisition.validate.ReasonsValidator;
import org.slf4j.ext.XLogger;
import org.slf4j.ext.XLoggerFactory;
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

  private static final XLogger XLOGGER = XLoggerFactory.getXLogger(RequisitionController.class);
  private static final String CHECK_PERMISSION = "CHECK_PERMISSION";
  private static final String GET_REQUISITION_BY_ID = "GET_REQUISITION_BY_ID";
  private static final String VALIDATE_CAN_CHANGE_STATUS = "VALIDATE_CAN_CHANGE_STATUS";
  private static final String CHECK_IF_PERIOD_IS_VALID = "CHECK_IF_PERIOD_IS_VALID";
  private static final String GET_CURRENT_USER = "GET_CURRENT_USER";
  private static final String GET_ORDERABLES_FOR_LINE_ITEMS = "GET_ORDERABLES_FOR_LINE_ITEMS";
  private static final String SAVE = "SAVE";
  private static final String CALL_STATUS_CHANGE_PROCESSOR = "CALL_STATUS_CHANGE_PROCESSOR";
  private static final String BUILD_BASIC_REQUISITION_DTO = "BUILD_BASIC_REQUISITION_DTO";
  private static final String BUILD_DTO_LIST = "BUILD_DTO_LIST";

  @Autowired
  private PeriodService periodService;

  @Autowired
  private UserFulfillmentFacilitiesReferenceDataService fulfillmentFacilitiesReferenceDataService;

  @Autowired
  private RequisitionStatusNotifier requisitionStatusNotifier;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private ValidReasonStockmanagementService validReasonStockmanagementService;

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
    XLOGGER.entry(programId, facilityId, suggestedPeriod, emergency);
    Profiler profiler = new Profiler("POST_REQUISITION_INITIATE");
    profiler.setLogger(XLOGGER);

    if (null == facilityId || null == programId) {
      throw new ValidationMessageException(
          new Message(MessageKeys.ERROR_INITIALIZE_MISSING_PARAMETERS));
    }

    profiler.start("GET_FACILITY");
    FacilityDto facility = facilityReferenceDataService.findOne(facilityId);
    if (facility == null) {
      throw new ContentNotFoundMessageException(
          new Message(MessageKeys.ERROR_FACILITY_NOT_FOUND, facilityId));
    }

    profiler.start("GET_PROGRAM");
    ProgramDto program = programReferenceDataService.findOne(programId);
    if (program == null) {
      throw new ContentNotFoundMessageException(
          new Message(MessageKeys.ERROR_PROGRAM_NOT_FOUND, programId));
    }

    profiler.start("CHECK_PERM_TO_INITIATE");
    permissionService.canInitRequisition(programId, facilityId).throwExceptionIfHasErrors();

    profiler.start("CHECK_FACILITY_SUPPORTS_PROGRAM");
    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facility, programId);

    profiler.start("GET_STOCK_ADJ_REASONS");
    List<StockAdjustmentReason> stockAdjustmentReasons =
        getStockAdjustmentReasons(programId, facility);

    profiler.start("INITIATE_REQUISITION");
    Requisition newRequisition = requisitionService.initiate(
        program, facility, suggestedPeriod, emergency, stockAdjustmentReasons);

    profiler.start("VALIDATE_REASONS");
    reasonsValidator.validate(stockAdjustmentReasons, newRequisition.getTemplate());

    profiler.start("Build DTO");
    RequisitionDto requisitionDto = requisitionDtoBuilder.build(newRequisition, facility, program);

    profiler.stop().log();
    XLOGGER.exit(requisitionDto);
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

    profiler.start(CHECK_PERMISSION);
    permissionService.canInitOrAuthorizeRequisition(programId, facilityId)
        .throwExceptionIfHasErrors();

    profiler.start("CHECK_IF_FACILITY_SUPPORTS_PROGRAM");
    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facilityId, programId);

    profiler.start("GET_PERIODS");
    Collection<ProcessingPeriodDto> periods = periodService.getPeriods(
        programId, facilityId, emergency
    );
    profiler.stop().log();
    XLOGGER.exit(periods);
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

    profiler.start(CHECK_PERMISSION);
    permissionService.canSubmitRequisition(requisitionId).throwExceptionIfHasErrors();

    profiler.start(GET_REQUISITION_BY_ID);
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      throw new ContentNotFoundMessageException(
          new Message(MessageKeys.ERROR_REQUISITION_NOT_FOUND, requisitionId));
    }

    profiler.start(VALIDATE_CAN_CHANGE_STATUS);
    validateForStatusChange(requisition);

    profiler.start(CHECK_IF_PERIOD_IS_VALID);
    checkIfPeriodIsValid(requisition);

    logger.debug("Submitting a requisition with id " + requisition.getId());

    profiler.start("GET_REQUISITION_PROGRAM");
    ProgramDto program = programReferenceDataService.findOne(requisition.getProgramId());

    profiler.start(GET_ORDERABLES_FOR_LINE_ITEMS);
    List<OrderableDto> orderableIds = orderableReferenceDataService.findByIds(
        getLineItemOrderableIds(requisition));

    profiler.start("SUBMIT");
    requisition.submit(orderableIds, getCurrentUser(profiler).getId(),
        program.getSkipAuthorization());

    profiler.start(SAVE);
    requisitionService.saveStatusMessage(requisition);
    requisitionRepository.save(requisition);

    callStatusChangeProcessor(profiler, requisition);
    logger.debug("Requisition with id " + requisition.getId() + " submitted");

    BasicRequisitionDto dto = buildBasicRequisitionDto(profiler, requisition);

    profiler.stop().log();
    XLOGGER.exit(dto);
    return dto;
  }

  /**
   * Deletes requisition with the given id.
   */
  @RequestMapping(value = "/requisitions/{id}", method = RequestMethod.DELETE)
  @ResponseStatus(HttpStatus.NO_CONTENT)
  public void deleteRequisition(@PathVariable("id") UUID requisitionId) {
    Profiler profiler = getProfiler("DELETE_REQUISITION", requisitionId);

    profiler.start(CHECK_PERMISSION);
    permissionService.canDeleteRequisition(requisitionId).throwExceptionIfHasErrors();

    profiler.start("DELETE");
    requisitionService.delete(requisitionId);

    profiler.stop().log();
    XLOGGER.exit();
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

    if (isNotTrue(isNull(requisitionDto.getId()))
        && isNotTrue(requisitionId.equals(requisitionDto.getId()))) {
      throw new ValidationMessageException(ERROR_ID_MISMATCH);
    }

    profiler.start(CHECK_PERMISSION);
    requisitionService.validateCanSaveRequisition(requisitionId)
        .throwExceptionIfHasErrors();

    profiler.start(GET_REQUISITION_BY_ID);
    Requisition requisitionToUpdate = requisitionRepository.findOne(requisitionId);

    profiler.start(GET_ORDERABLES_FOR_LINE_ITEMS);
    Map<UUID, OrderableDto> orderables = orderableReferenceDataService
        .findByIds(requisitionToUpdate.getAllOrderableIds())
        .stream()
        .collect(Collectors.toMap(OrderableDto::getId, Function.identity()));

    profiler.start("BUILD_REQUISITION_UPDATER");
    Requisition requisition = RequisitionBuilder.newRequisition(requisitionDto,
        requisitionToUpdate.getTemplate(), requisitionToUpdate.getProgramId(),
        requisitionToUpdate.getStatus(), orderables);
    requisition.setId(requisitionId);

    profiler.start("VALIDATE_TIMESTAMPS");
    requisitionVersionValidator.validateRequisitionTimestamps(requisition, requisitionToUpdate)
        .throwExceptionIfHasErrors();
    profiler.start("VALIDATE_CAN_BE_UPDATED");
    validateRequisitionCanBeUpdated(requisitionToUpdate, requisition)
        .throwExceptionIfHasErrors();

    logger.debug("Updating requisition with id: {}", requisitionId);

    profiler.start("UPDATE");
    RequisitionDto dto = doUpdate(requisitionToUpdate, requisition);

    profiler.stop().log();
    XLOGGER.exit(dto);
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

    profiler.start("CHECK_PERM_REQUISITION_VIEW");
    permissionService.canViewRequisition(requisitionId).throwExceptionIfHasErrors();

    profiler.start("FIND_ONE_REQUISITION");
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      profiler.stop().log();
      throw new ContentNotFoundMessageException(
          new Message(MessageKeys.ERROR_REQUISITION_NOT_FOUND, requisitionId));
    } else {
      profiler.start("REQUISITION_DTO_BUILD");
      RequisitionDto requisitionDto = requisitionDtoBuilder.build(requisition);
      
      profiler.stop().log();
      XLOGGER.exit(requisitionDto);
      return requisitionDto;
    }
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
    Profiler profiler = getProfiler("REQUISITIONS_SEARCH", facility, program,
        initiatedDateFrom, initiatedDateTo, processingPeriod, supervisoryNode, requisitionStatuses,
        pageable);

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

    profiler.stop().log();
    XLOGGER.exit(requisitionDtoPage);
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

    profiler.start(CHECK_PERMISSION);
    permissionService.canUpdateRequisition(requisitionId).throwExceptionIfHasErrors();

    profiler.start("SKIP");
    Requisition requisition = requisitionService.skip(requisitionId);

    callStatusChangeProcessor(profiler, requisition);

    BasicRequisitionDto dto = buildBasicRequisitionDto(profiler, requisition);

    profiler.stop().log();
    XLOGGER.exit(dto);
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

    profiler.start(CHECK_PERMISSION);
    permissionService.canApproveRequisition(requisitionId).throwExceptionIfHasErrors();

    profiler.start("REJECT");
    Requisition rejectedRequisition = requisitionService.reject(requisitionId);

    callStatusChangeProcessor(profiler, rejectedRequisition);

    profiler.start("NOTIFY_STATUS_CHANGED");
    requisitionStatusNotifier.notifyStatusChanged(rejectedRequisition);

    BasicRequisitionDto dto = buildBasicRequisitionDto(profiler, rejectedRequisition);

    profiler.stop().log();
    XLOGGER.exit(dto);
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

    profiler.start("FIND_ONE_REQUISITION");
    Requisition requisition = requisitionRepository.findOne(requisitionId);

    profiler.start(GET_CURRENT_USER);
    UserDto user = authenticationHelper.getCurrentUser();

    profiler.start("CHECK_PERM_REQUISITION_APPROVE");
    requisitionService.validateCanApproveRequisition(requisition, requisitionId, user.getId())
        .throwExceptionIfHasErrors();

    profiler.start("VALIDATE_REQUISITION");
    validateForStatusChange(requisition);

    profiler.start("DO_APPROVE");
    BasicRequisitionDto requisitionDto = doApprove(requisition, user);
    
    profiler.stop().log();
    XLOGGER.exit(requisitionDto);
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

    profiler.start(GET_CURRENT_USER);
    UserDto user = authenticationHelper.getCurrentUser();

    profiler.start("REQUISITION_SERVICE_GET_FOR_APPROVAL");
    Page<Requisition> approvalRequisitions = requisitionService
        .getRequisitionsForApproval(user.getId(), programId, pageable);

    profiler.start(BUILD_DTO_LIST);
    Page<BasicRequisitionDto> dtoPage = Pagination.getPage(
        basicRequisitionDtoBuilder.build(approvalRequisitions.getContent()),
        pageable,
        approvalRequisitions.getTotalElements());

    profiler.stop().log();
    XLOGGER.exit();
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

    profiler.stop().log();
    XLOGGER.exit();
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

    profiler.start(CHECK_PERMISSION);
    permissionService.canAuthorizeRequisition(requisitionId).throwExceptionIfHasErrors();

    profiler.start(GET_REQUISITION_BY_ID);
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      throw new ContentNotFoundMessageException(
          new Message(MessageKeys.ERROR_REQUISITION_NOT_FOUND, requisitionId));
    }

    profiler.start(VALIDATE_CAN_CHANGE_STATUS);
    validateForStatusChange(requisition);

    profiler.start(CHECK_IF_PERIOD_IS_VALID);
    checkIfPeriodIsValid(requisition);

    profiler.start(GET_CURRENT_USER);
    UserDto user = authenticationHelper.getCurrentUser();

    profiler.start(GET_ORDERABLES_FOR_LINE_ITEMS);
    List<OrderableDto> orderableIds = orderableReferenceDataService.findByIds(
        getLineItemOrderableIds(requisition));

    profiler.start("AUTHORIZE");
    requisition.authorize(orderableIds, user.getId());

    profiler.start(SAVE);
    requisitionService.saveStatusMessage(requisition);
    requisitionRepository.save(requisition);

    callStatusChangeProcessor(profiler, requisition);
    logger.debug("Requisition: " + requisitionId + " authorized.");

    BasicRequisitionDto dto = buildBasicRequisitionDto(profiler, requisition);

    profiler.stop().log();
    XLOGGER.exit(dto);
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
    Profiler profiler = getProfiler("GET_REQUISITIONS_FOR_CONVERT", filterBy, filterValue,
        pageable);

    profiler.start(GET_CURRENT_USER);
    UserDto user = authenticationHelper.getCurrentUser();

    profiler.start("GET_RIGHT");
    RightDto right = authenticationHelper.getRight(RightName.ORDERS_EDIT);

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

    profiler.stop().log();
    XLOGGER.exit(page);
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

    profiler.start(CHECK_PERMISSION);
    permissionService.canConvertToOrder(list).throwExceptionIfHasErrors();

    profiler.start("CONVERT");
    requisitionService.convertToOrder(list, getCurrentUser(profiler));

    profiler.stop().log();
    XLOGGER.exit();
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

  private Profiler getProfiler(String name, Object... entryArgs) {
    XLOGGER.entry(entryArgs);
    Profiler profiler = new Profiler(name);
    profiler.setLogger(XLOGGER);
    return profiler;
  }

  private UserDto getCurrentUser(Profiler profiler) {
    profiler.start(GET_CURRENT_USER);
    return authenticationHelper.getCurrentUser();
  }

  private void callStatusChangeProcessor(Profiler profiler, Requisition requisition) {
    profiler.start(CALL_STATUS_CHANGE_PROCESSOR);
    requisitionStatusProcessor.statusChange(requisition);
  }

  private BasicRequisitionDto buildBasicRequisitionDto(Profiler profiler, Requisition requisition) {
    profiler.start(BUILD_BASIC_REQUISITION_DTO);
    return basicRequisitionDtoBuilder.build(requisition);
  }
}
