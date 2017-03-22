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
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_AUTHORIZATION_TO_BE_SKIPPED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_CANNOT_UPDATE_WITH_STATUS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_ID_MISMATCH;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REQUISITION_NOT_FOUND;
import static org.springframework.web.bind.annotation.RequestMethod.GET;
import static org.springframework.web.bind.annotation.RequestMethod.POST;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionBuilder;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusMessage;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.BindingResultException;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.RequisitionStatusProcessor;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.StockAdjustmentReasonReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.requisition.validate.DraftRequisitionValidator;
import org.openlmis.requisition.validate.RequisitionValidator;
import org.openlmis.requisition.validate.RequisitionVersionValidator;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.AuthenticationHelper;
import org.openlmis.utils.FacilitySupportsProgramHelper;
import org.openlmis.utils.Message;
import org.openlmis.utils.Pagination;
import org.openlmis.utils.RightName;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

import java.time.ZonedDateTime;
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@SuppressWarnings("PMD.TooManyMethods")
@Controller
@Transactional
public class RequisitionController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionController.class);
  private static final String REQUISITION = "requisition";

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private RequisitionValidator validator;

  @Autowired
  private DraftRequisitionValidator draftValidator;

  @Autowired
  private RequisitionService requisitionService;

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  @Autowired
  private PeriodService periodService;

  @Autowired
  private UserFulfillmentFacilitiesReferenceDataService fulfillmentFacilitiesReferenceDataService;

  @Autowired
  private StockAdjustmentReasonReferenceDataService stockAdjustmentReasonReferenceDataService;

  @Autowired
  private SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

  @Autowired
  private AuthenticationHelper authenticationHelper;

  @Autowired
  private PermissionService permissionService;

  @Autowired
  private RequisitionDtoBuilder requisitionDtoBuilder;

  @Autowired
  private FacilitySupportsProgramHelper facilitySupportsProgramHelper;

  @Autowired
  private OrderableReferenceDataService orderableReferenceDataService;
  
  @Autowired
  private StatusMessageRepository statusMessageRepository;

  @Autowired
  private RequisitionVersionValidator requisitionVersionValidator;

  @Autowired
  private RequisitionStatusProcessor requisitionStatusProcessor;

  /**
   * Allows creating new requisitions.
   *
   * @param program         UUID of Program.
   * @param facility        UUID of Facility.
   * @param emergency       Emergency status.
   * @param suggestedPeriod Period for requisition.
   * @return created requisition.
   */
  @RequestMapping(value = "/requisitions/initiate", method = POST)
  @ResponseStatus(HttpStatus.CREATED)
  @ResponseBody
  public RequisitionDto initiate(@RequestParam(value = "program") UUID program,
                    @RequestParam(value = "facility") UUID facility,
                    @RequestParam(value = "suggestedPeriod", required = false) UUID suggestedPeriod,
                    @RequestParam(value = "emergency") boolean emergency) {
    if (null == facility || null == program) {
      throw new ValidationMessageException(
          new Message(MessageKeys.ERROR_INITIALIZE_MISSING_PARAMETERS));
    }

    permissionService.canInitRequisition(program, facility);
    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facility, program);

    Requisition newRequisition = requisitionService
        .initiate(program, facility, suggestedPeriod, emergency);
    return requisitionDtoBuilder.build(newRequisition);
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

    permissionService.canInitOrAuthorizeRequisition(program, facility);

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
  public RequisitionDto submitRequisition(@PathVariable("id") UUID requisitionId) {
    permissionService.canSubmitRequisition(requisitionId);
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      throw new ContentNotFoundMessageException(
          new Message(MessageKeys.ERROR_REQUISITION_NOT_FOUND, requisitionId));
    }

    BindingResult bindingResult = new BeanPropertyBindingResult(requisition, REQUISITION);
    validator.validate(requisition, bindingResult);

    if (bindingResult.hasErrors()) {
      LOGGER.warn("Validation for requisition failed: {}", getErrors(bindingResult));
      throw new BindingResultException(getErrors(bindingResult));
    }

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(requisition.getFacilityId(),
        requisition.getProgramId());

    LOGGER.debug("Submitting a requisition with id " + requisition.getId());

    UserDto user = authenticationHelper.getCurrentUser();
    requisition.submit(orderableReferenceDataService.findAll(), user.getId());
    saveStatusMessage(requisition);
    
    requisitionService.saveRequisitionWithStatusChange(requisition);
    requisitionStatusProcessor.statusChange(requisition);
    LOGGER.debug("Requisition with id " + requisition.getId() + " submitted");

    return requisitionDtoBuilder.build(requisition);
  }

  /**
   * Deletes requisition with the given id.
   */
  @RequestMapping(value = "/requisitions/{id}", method = RequestMethod.DELETE)
  @ResponseStatus(HttpStatus.NO_CONTENT)
  public void deleteRequisition(@PathVariable("id") UUID requisitionId) {
    permissionService.canDeleteRequisition(requisitionId);
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
    permissionService.canUpdateRequisition(requisitionId);

    if (isNotTrue(isNull(requisitionDto.getId()))
        && isNotTrue(requisitionId.equals(requisitionDto.getId()))) {
      throw new ValidationMessageException(new Message(ERROR_ID_MISMATCH));
    }

    Requisition requisitionToUpdate = requisitionRepository.findOne(requisitionId);

    if (isNull(requisitionToUpdate)) {
      throw new ContentNotFoundMessageException(new Message(
          ERROR_REQUISITION_NOT_FOUND, requisitionId));
    }

    Requisition requisition = RequisitionBuilder.newRequisition(requisitionDto,
        requisitionToUpdate.getTemplate(), requisitionToUpdate.getProgramId(),
        requisitionToUpdate.getStatus());
    requisition.setId(requisitionId);

    requisitionVersionValidator.validateRequisitionTimestamps(
        requisition, requisitionToUpdate
    );

    RequisitionStatus status = requisitionToUpdate.getStatus();
    if (status != RequisitionStatus.APPROVED
        && status != RequisitionStatus.SKIPPED
        && status != RequisitionStatus.RELEASED) {
      LOGGER.debug("Updating requisition with id: {}", requisitionId);

      BindingResult bindingResult = new BeanPropertyBindingResult(requisition, REQUISITION);
      draftValidator.validate(requisition, bindingResult);

      if (bindingResult.hasErrors()) {
        LOGGER.warn("Validation for requisition failed: {}", getErrors(bindingResult));
        throw new BindingResultException(getErrors(bindingResult));
      }

      requisitionToUpdate.updateFrom(requisition,
          stockAdjustmentReasonReferenceDataService.getStockAdjustmentReasonsByProgram(
              requisitionToUpdate.getProgramId()), orderableReferenceDataService.findAll());

      requisitionToUpdate = requisitionRepository.save(requisitionToUpdate);

      LOGGER.debug("Saved requisition with id: " + requisitionToUpdate.getId());
      return requisitionDtoBuilder.build(requisitionToUpdate);
    } else {
      throw new ValidationMessageException(new Message(ERROR_CANNOT_UPDATE_WITH_STATUS,
          requisitionToUpdate.getStatus()));
    }
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
    permissionService.canViewRequisition(requisitionId);
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
  public Page<RequisitionDto> searchRequisitions(
      @RequestParam(value = "facility", required = false) UUID facility,
      @RequestParam(value = "program", required = false) UUID program,
      @RequestParam(value = "initiatedDateFrom", required = false)
      @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) ZonedDateTime initiatedDateFrom,
      @RequestParam(value = "initiatedDateTo", required = false)
      @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) ZonedDateTime initiatedDateTo,
      @RequestParam(value = "processingPeriod", required = false)
          UUID processingPeriod,
      @RequestParam(value = "supervisoryNode", required = false) UUID supervisoryNode,
      @RequestParam(value = "requisitionStatus", required = false)
          Set<RequisitionStatus> requisitionStatuses,
      @RequestParam(value = "emergency", required = false) Boolean emergency,
      Pageable pageable) {
    Page<Requisition> requisitionsPage = requisitionService.searchRequisitions(facility, program,
        initiatedDateFrom, initiatedDateTo, processingPeriod, supervisoryNode, requisitionStatuses,
        emergency, pageable);
    List<Requisition> resultList = requisitionsPage.getContent();

    List<Requisition> filteredList = resultList.stream().filter(req -> {
      try {
        permissionService.canViewRequisition(req.getId());
      } catch (PermissionMessageException ex) {
        return false;
      }
      return true;
    }).collect(Collectors.toList());

    List<RequisitionDto> dtoList = requisitionDtoBuilder.build(filteredList);
    return Pagination.getPage(dtoList, pageable, dtoList.size());
  }

  /**
   * Skipping chosen requisition period.
   */
  @RequestMapping(value = "/requisitions/{id}/skip", method = RequestMethod.PUT)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public RequisitionDto skipRequisition(@PathVariable("id") UUID requisitionId) {
    permissionService.canUpdateRequisition(requisitionId);

    Requisition requisition = requisitionService.skip(requisitionId);
    requisitionStatusProcessor.statusChange(requisition);
    return requisitionDtoBuilder.build(requisition);
  }

  /**
   * Rejecting requisition which is waiting for approve.
   */
  @RequestMapping(value = "/requisitions/{id}/reject", method = RequestMethod.PUT)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public RequisitionDto rejectRequisition(@PathVariable("id") UUID id) {
    permissionService.canApproveRequisition(id);
    Requisition rejectedRequisition = requisitionService.reject(id);
    requisitionStatusProcessor.statusChange(rejectedRequisition);

    return requisitionDtoBuilder.build(rejectedRequisition);
  }

  /**
   * Approve specified by id requisition.
   */
  @RequestMapping(value = "/requisitions/{id}/approve", method = RequestMethod.POST)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public RequisitionDto approveRequisition(@PathVariable("id") UUID requisitionId) {
    permissionService.canApproveRequisition(requisitionId);
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      throw new ContentNotFoundMessageException(
          new Message(MessageKeys.ERROR_REQUISITION_NOT_FOUND, requisitionId));
    }

    BindingResult bindingResult = new BeanPropertyBindingResult(requisition, REQUISITION);
    validator.validate(requisition, bindingResult);

    if (bindingResult.hasErrors()) {
      LOGGER.warn("Validation for requisition failed: {}", getErrors(bindingResult));
      throw new BindingResultException(getErrors(bindingResult));
    }
    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(requisition.getFacilityId(),
        requisition.getProgramId());

    if (requisition.isApprovable()
        || (configurationSettingService.getBoolValue("skipAuthorization")
        && requisition.getStatus() == RequisitionStatus.SUBMITTED)) {

      SupervisoryNodeDto supervisoryNodeDto =
          supervisoryNodeReferenceDataService.findOne(requisition.getSupervisoryNodeId());
      UUID parentNodeId = null;
      if (supervisoryNodeDto != null) {
        SupervisoryNodeDto parentNode = supervisoryNodeDto.getParentNode();
        if (parentNode != null) {
          parentNodeId = parentNode.getId();
        }
      }
      requisition.approve(parentNodeId, orderableReferenceDataService.findAll());

      saveStatusMessage(requisition);

      requisitionService.saveRequisitionWithStatusChange(requisition);
      requisitionStatusProcessor.statusChange(requisition);
      LOGGER.debug("Requisition with id " + requisitionId + " approved");
      return requisitionDtoBuilder.build(requisition);
    } else {
      throw new ValidationMessageException(new Message(
          MessageKeys.ERROR_REQUISITION_MUST_BE_AUTHORIZED_OR_SUBMITTED, requisitionId));
    }
  }

  /**
   * Get requisitions to approve for right supervisor.
   *
   * @return Approved requisitions.
   */
  @RequestMapping(value = "/requisitions/requisitionsForApproval", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public Page<RequisitionDto> requisitionsForApproval(Pageable pageable) {
    UserDto user = authenticationHelper.getCurrentUser();
    Set<Requisition> approvalRequisitions = requisitionService
        .getRequisitionsForApproval(user.getId());

    List<RequisitionDto> dtoList = requisitionDtoBuilder.build(approvalRequisitions);
    return Pagination.getPage(dtoList, pageable);
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
    Page<Requisition> submittedRequisitionsPage = requisitionService.searchRequisitions(
        EnumSet.of(RequisitionStatus.SUBMITTED), pageable);

    List<Requisition> submittedRequisitions = submittedRequisitionsPage.getContent();
    List<Requisition> filteredList = submittedRequisitions.stream().filter(req -> {
      try {
        permissionService.canViewRequisition(req.getId());
      } catch (PermissionMessageException ex) {
        return false;
      }
      return true;
    }).collect(Collectors.toList());

    List<RequisitionDto> dtoList = requisitionDtoBuilder.build(filteredList);

    return Pagination.getPage(dtoList, pageable, dtoList.size());
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
  public RequisitionDto authorizeRequisition(@PathVariable("id") UUID requisitionId) {
    permissionService.canAuthorizeRequisition(requisitionId);

    if (configurationSettingService.getBoolValue("skipAuthorization")) {
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
    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(requisition.getFacilityId(),
        requisition.getProgramId());

    UserDto user = authenticationHelper.getCurrentUser();
    requisition.authorize(orderableReferenceDataService.findAll(), user.getId());

    UUID supervisoryNode = supervisoryNodeReferenceDataService.findSupervisoryNode(
        requisition.getProgramId(), requisition.getFacilityId()).getId();
    requisition.setSupervisoryNodeId(supervisoryNode);

    saveStatusMessage(requisition);

    requisitionService.saveRequisitionWithStatusChange(requisition);
    requisitionStatusProcessor.statusChange(requisition);
    LOGGER.debug("Requisition: " + requisitionId + " authorized.");

    return requisitionDtoBuilder.build(requisition);
  }

  /**
   * Get approved requisitions matching all of provided parameters.
   *
   * @param filterValue Value to be used to filter.
   * @param filterBy    Field used to filter: "programName", "facilityCode", "facilityName" or
   *                    "all".
   * @param sortBy      Field used to sort: "programName", "facilityCode" or "facilityName".
   * @param descending  Descending direction for sort.
   * @param pageable     Pageable object that allows client to optionally add "page" (page number)
   *                     and "size" (page size) query parameters to the request.
   * @return Page of approved requisitions.
   */
  @RequestMapping(value = "/requisitions/requisitionsForConvert", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public Page<RequisitionWithSupplyingDepotsDto> listForConvertToOrder(
      @RequestParam(required = false) String filterValue,
      @RequestParam(required = false) String filterBy,
      @RequestParam(required = false, defaultValue = "programName") String sortBy,
      @RequestParam(required = false, defaultValue = "true") boolean descending,
      Pageable pageable) {
    UserDto user = authenticationHelper.getCurrentUser();
    RightDto right = authenticationHelper.getRight(RightName.ORDERS_EDIT);

    Collection<UUID> userManagedFacilities = fulfillmentFacilitiesReferenceDataService
        .getFulfillmentFacilities(user.getId(), right.getId())
        .stream().map(FacilityDto::getId).collect(Collectors.toList());

    return requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
            filterValue, filterBy, sortBy, descending, pageable, userManagedFacilities);
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
    permissionService.canConvertToOrder(list);
    requisitionService.convertToOrder(list, user);
  }

  @InitBinder("requisition")
  protected void initBinder(final WebDataBinder binder) {
    binder.addValidators(validator);
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
