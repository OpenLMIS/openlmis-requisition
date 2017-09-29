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
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_AUTHORIZATION_TO_BE_SKIPPED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_ID_MISMATCH;
import static org.springframework.web.bind.annotation.RequestMethod.GET;
import static org.springframework.web.bind.annotation.RequestMethod.POST;

import java.time.LocalDate;
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionBuilder;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StockAdjustmentReason;
import org.openlmis.requisition.dto.BasicRequisitionDto;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ReasonDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.dto.ValidReasonDto;
import org.openlmis.requisition.exception.BindingResultException;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.RequisitionStatusNotifier;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.ValidReasonStockmanagementService;
import org.openlmis.requisition.settings.service.ConfigurationSettingService;
import org.openlmis.requisition.utils.FacilitySupportsProgramHelper;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.utils.Pagination;
import org.openlmis.requisition.utils.RightName;
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
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.InitBinder;
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

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  @Autowired
  private PeriodService periodService;

  @Autowired
  private UserFulfillmentFacilitiesReferenceDataService fulfillmentFacilitiesReferenceDataService;

  @Autowired
  private FacilitySupportsProgramHelper facilitySupportsProgramHelper;

  @Autowired
  private RequisitionStatusNotifier requisitionStatusNotifier;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private ValidReasonStockmanagementService validReasonStockmanagementService;

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
    if (null == facilityId || null == programId) {
      throw new ValidationMessageException(
          new Message(MessageKeys.ERROR_INITIALIZE_MISSING_PARAMETERS));
    }

    FacilityDto facility = facilityReferenceDataService.findOne(facilityId);
    if (facility == null) {
      throw new ContentNotFoundMessageException(
          new Message(MessageKeys.ERROR_FACILITY_NOT_FOUND, facilityId));
    }
    ProgramDto program = programReferenceDataService.findOne(programId);
    if (program == null) {
      throw new ContentNotFoundMessageException(
          new Message(MessageKeys.ERROR_PROGRAM_NOT_FOUND, programId));
    }

    permissionService.canInitRequisition(programId, facilityId).throwExceptionIfHasErrors();
    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facility, programId);

    List<StockAdjustmentReason> stockAdjustmentReasons =
        getStockAdjustmentReasons(programId, facility);

    Requisition newRequisition = requisitionService.initiate(
        program.getId(), facility.getId(), suggestedPeriod, emergency, stockAdjustmentReasons);
    return requisitionDtoBuilder.build(newRequisition, facility, program);
  }

  /**
   * Returns processing periods for unprocessed requisitions.
   *
   * @param program   UUID of the Program.
   * @param facility  UUID of the Facility.
   * @param emergency true for periods to initiate an emergency requisition; false otherwise.
   * @return processing periods.
   */
  @RequestMapping(value = "/requisitions/periodsForInitiate", method = GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public Collection<ProcessingPeriodDto> getProcessingPeriodIds(
      @RequestParam(value = "programId") UUID program,
      @RequestParam(value = "facilityId") UUID facility,
      @RequestParam(value = "emergency") boolean emergency) {
    if (null == facility || null == program) {
      throw new ValidationMessageException(
          new Message(MessageKeys.ERROR_REQUISITION_PERIODS_FOR_INITIATE_MISSING_PARAMETERS));
    }

    permissionService.canInitOrAuthorizeRequisition(program, facility).throwExceptionIfHasErrors();

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facility, program);

    return periodService.getPeriods(
        program, facility, emergency
    );
  }

  /**
   * Submits earlier initiated requisition.
   */
  @RequestMapping(value = "/requisitions/{id}/submit", method = RequestMethod.POST)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public BasicRequisitionDto submitRequisition(@PathVariable("id") UUID requisitionId) {
    permissionService.canSubmitRequisition(requisitionId).throwExceptionIfHasErrors();
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      throw new ContentNotFoundMessageException(
          new Message(MessageKeys.ERROR_REQUISITION_NOT_FOUND, requisitionId));
    }

    BindingResult bindingResult = new BeanPropertyBindingResult(requisition, REQUISITION);
    validator.validate(requisition, bindingResult);

    if (bindingResult.hasErrors()) {
      logger.warn("Validation for requisition failed: {}", getErrors(bindingResult));
      throw new BindingResultException(getErrors(bindingResult));
    }

    checkIfPeriodIsValid(requisition);

    logger.debug("Submitting a requisition with id " + requisition.getId());

    UserDto user = authenticationHelper.getCurrentUser();

    requisition.submit(orderableReferenceDataService.findByIds(
            getLineItemOrderableIds(requisition)), user.getId());
    saveStatusMessage(requisition);

    requisitionRepository.save(requisition);
    requisitionStatusProcessor.statusChange(requisition);
    logger.debug("Requisition with id " + requisition.getId() + " submitted");

    return basicRequisitionDtoBuilder.build(requisition);
  }

  /**
   * Deletes requisition with the given id.
   */
  @RequestMapping(value = "/requisitions/{id}", method = RequestMethod.DELETE)
  @ResponseStatus(HttpStatus.NO_CONTENT)
  public void deleteRequisition(@PathVariable("id") UUID requisitionId) {
    permissionService.canDeleteRequisition(requisitionId).throwExceptionIfHasErrors();
    requisitionService.delete(requisitionId);
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
    if (isNotTrue(isNull(requisitionDto.getId()))
        && isNotTrue(requisitionId.equals(requisitionDto.getId()))) {
      throw new ValidationMessageException(ERROR_ID_MISMATCH);
    }

    requisitionService.validateCanSaveRequisition(requisitionId)
        .throwExceptionIfHasErrors();

    Requisition requisitionToUpdate = requisitionRepository.findOne(requisitionId);
    Requisition requisition = RequisitionBuilder.newRequisition(requisitionDto,
        requisitionToUpdate.getTemplate(), requisitionToUpdate.getProgramId(),
        requisitionToUpdate.getStatus());
    requisition.setId(requisitionId);

    requisitionVersionValidator.validateRequisitionTimestamps(requisition, requisitionToUpdate)
        .throwExceptionIfHasErrors();
    validateFields(draftValidator, requisition).throwExceptionIfHasErrors();

    logger.debug("Updating requisition with id: {}", requisitionId);

    return doUpdate(requisitionToUpdate, requisition);
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
    permissionService.canViewRequisition(requisitionId).throwExceptionIfHasErrors();
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      throw new ContentNotFoundMessageException(
          new Message(MessageKeys.ERROR_REQUISITION_NOT_FOUND, requisitionId));
    } else {
      return requisitionDtoBuilder.build(requisition);
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
    XLOGGER.entry(facility, program, initiatedDateFrom, initiatedDateTo, processingPeriod, 
        supervisoryNode, requisitionStatuses, pageable);
    Profiler profiler = new Profiler("REQUISITIONS_SEARCH");
    profiler.setLogger(XLOGGER);

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
    permissionService.canUpdateRequisition(requisitionId).throwExceptionIfHasErrors();

    Requisition requisition = requisitionService.skip(requisitionId);
    requisitionStatusProcessor.statusChange(requisition);
    return basicRequisitionDtoBuilder.build(requisition);
  }

  /**
   * Rejecting requisition which is waiting for approve.
   */
  @RequestMapping(value = "/requisitions/{id}/reject", method = RequestMethod.PUT)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public BasicRequisitionDto rejectRequisition(@PathVariable("id") UUID requisitionId) {
    permissionService.canApproveRequisition(requisitionId).throwExceptionIfHasErrors();
    Requisition rejectedRequisition = requisitionService.reject(requisitionId);
    requisitionStatusProcessor.statusChange(rejectedRequisition);

    requisitionStatusNotifier.notifyStatusChanged(rejectedRequisition);

    return basicRequisitionDtoBuilder.build(rejectedRequisition);
  }

  /**
   * Approve specified by id requisition.
   */
  @RequestMapping(value = "/requisitions/{id}/approve", method = RequestMethod.POST)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public BasicRequisitionDto approveRequisition(@PathVariable("id") UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    UUID userId = authenticationHelper.getCurrentUser().getId();

    requisitionService.validateCanApproveRequisition(requisition, requisitionId, userId)
        .throwExceptionIfHasErrors();
    validateFields(validator, requisition).throwExceptionIfHasErrors();

    return doApprove(requisition, userId);
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
    XLOGGER.entry(programId, pageable);
    Profiler profiler = new Profiler("REQUISITIONS_FOR_APPROVAL");
    profiler.setLogger(XLOGGER);

    profiler.start("GET_USER");
    UserDto user = authenticationHelper.getCurrentUser();

    profiler.start("REQUISITION_SERVICE_GET_FOR_APPROVAL");
    Page<Requisition> approvalRequisitions = requisitionService
        .getRequisitionsForApproval(user.getId(), programId, pageable);

    profiler.start("BUILD_DTO_LIST");
    Page<BasicRequisitionDto> dtoPage = Pagination.getPage(
        basicRequisitionDtoBuilder.build(approvalRequisitions.getContent()), pageable);

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

    Page<Requisition> submittedRequisitions = requisitionService.searchRequisitions(
        EnumSet.of(RequisitionStatus.SUBMITTED), pageable);
    
    return Pagination.getPage(
        requisitionDtoBuilder.build(submittedRequisitions.getContent()),
        pageable,
        submittedRequisitions.getTotalElements());
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
    permissionService.canAuthorizeRequisition(requisitionId).throwExceptionIfHasErrors();

    if (configurationSettingService.getSkipAuthorization()) {
      throw new ValidationMessageException(new Message(ERROR_AUTHORIZATION_TO_BE_SKIPPED));
    }

    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (requisition == null) {
      throw new ContentNotFoundMessageException(
          new Message(MessageKeys.ERROR_REQUISITION_NOT_FOUND, requisitionId));
    }

    BindingResult bindingResult = new BeanPropertyBindingResult(requisition, REQUISITION);
    validator.validate(requisition, bindingResult);

    if (bindingResult.hasErrors()) {
      throw new BindingResultException(getErrors(bindingResult));
    }

    checkIfPeriodIsValid(requisition);

    UserDto user = authenticationHelper.getCurrentUser();

    requisition.authorize(orderableReferenceDataService.findByIds(
            getLineItemOrderableIds(requisition)), user.getId());

    UUID supervisoryNode = supervisoryNodeReferenceDataService.findSupervisoryNode(
        requisition.getProgramId(), requisition.getFacilityId()).getId();
    requisition.setSupervisoryNodeId(supervisoryNode);

    saveStatusMessage(requisition);

    requisitionRepository.save(requisition);
    requisitionStatusProcessor.statusChange(requisition);
    logger.debug("Requisition: " + requisitionId + " authorized.");

    return basicRequisitionDtoBuilder.build(requisition);
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
    UserDto user = authenticationHelper.getCurrentUser();
    RightDto right = authenticationHelper.getRight(RightName.ORDERS_EDIT);

    Collection<UUID> userManagedFacilities = fulfillmentFacilitiesReferenceDataService
        .getFulfillmentFacilities(user.getId(), right.getId())
        .stream().map(FacilityDto::getId).collect(Collectors.toList());

    return requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
            filterValue, filterBy, pageable, userManagedFacilities);
  }

  /**
   * Converting Requisition list to orders.
   *
   * @param list List of Requisitions with their supplyingDepots that will be converted to Orders
   */
  @RequestMapping(value = "/requisitions/convertToOrder", method = RequestMethod.POST)
  @ResponseStatus(HttpStatus.CREATED)
  public void convertToOrder(@RequestBody List<ConvertToOrderDto> list) {
    UserDto user = authenticationHelper.getCurrentUser();
    permissionService.canConvertToOrder(list).throwExceptionIfHasErrors();
    requisitionService.convertToOrder(list, user);
  }

  @InitBinder("requisition")
  protected void initBinder(final WebDataBinder binder) {
    binder.addValidators(validator);
  }

  private List<StockAdjustmentReason> getStockAdjustmentReasons(UUID programId,
                                                                FacilityDto facilityDto) {
    List<ValidReasonDto> validReasons =
        validReasonStockmanagementService
            .search(programId, facilityDto.getType().getId());

    List<ReasonDto> reasonDtos = validReasons.stream()
        .map(ValidReasonDto::getReason)
        .collect(Collectors.toList());

    return StockAdjustmentReason.newInstance(reasonDtos);
  }
}
