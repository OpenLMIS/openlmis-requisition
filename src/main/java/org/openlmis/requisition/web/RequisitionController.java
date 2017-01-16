package org.openlmis.requisition.web;

import static org.springframework.web.bind.annotation.RequestMethod.GET;
import static org.springframework.web.bind.annotation.RequestMethod.POST;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionBuilder;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusMessage;
import org.openlmis.requisition.domain.Template;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionReportDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.JasperReportViewException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.openlmis.requisition.service.JasperReportsViewService;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.TemplateService;
import org.openlmis.requisition.service.referencedata.OrderableProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.StockAdjustmentReasonReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.requisition.validate.DraftRequisitionValidator;
import org.openlmis.requisition.validate.RequisitionValidator;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.AuthenticationHelper;
import org.openlmis.utils.FacilitySupportsProgramHelper;
import org.openlmis.utils.Message;
import org.openlmis.utils.ReportUtils;
import org.openlmis.utils.RequisitionExportHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
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
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.view.jasperreports.JasperReportsMultiFormatView;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;

@SuppressWarnings("PMD.TooManyMethods")
@Controller
public class RequisitionController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionController.class);
  private static final String REQUISITION = "requisition";
  private static final String REQUISITION_PRINT_TEMPLATE_NAME = "Print Requisition";

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
  private RequisitionExportHelper requisitionExportHelper;

  @Autowired
  private PermissionService permissionService;

  @Autowired
  private RequisitionDtoBuilder requisitionDtoBuilder;

  @Autowired
  private FacilitySupportsProgramHelper facilitySupportsProgramHelper;

  @Autowired
  private OrderableProductReferenceDataService orderableProductReferenceDataService;

  @Autowired
  private TemplateService templateService;

  @Autowired
  private JasperReportsViewService jasperReportsViewService;
  
  @Autowired
  private StatusMessageRepository statusMessageRepository;

  /**
   * Allows creating new requisitions.
   *
   * @param program         UUID of Program.
   * @param facility        UUID of Facility.
   * @param emergency       Emergency status.
   * @param suggestedPeriod Period for requisition.
   * @return ResponseEntity containing the created requisition
   */
  @RequestMapping(value = "/requisitions/initiate", method = POST)
  public ResponseEntity<?> initiate(@RequestParam(value = "program") UUID program,
                    @RequestParam(value = "facility") UUID facility,
                    @RequestParam(value = "suggestedPeriod", required = false) UUID suggestedPeriod,
                    @RequestParam(value = "emergency") boolean emergency) {
    if (null == facility || null == program) {
      throw new ValidationMessageException(
          new Message("requisition.error.initiate.missing-parameters"));
    }

    permissionService.canInitRequisition(program, facility);
    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facility, program);

    UserDto user = authenticationHelper.getCurrentUser();

    Requisition newRequisition = requisitionService
        .initiate(program, facility, suggestedPeriod, user.getId(), emergency);
    return new ResponseEntity<>(requisitionDtoBuilder.build(newRequisition), HttpStatus.CREATED);
  }

  /**
   * Returns processing periods for unprocessed requisitions.
   *
   * @param program   UUID of the Program.
   * @param facility  UUID of the Facility.
   * @param emergency true for periods to initiate an emergency requisition; false otherwise.
   * @return ResponseEntity containing processing periods
   */
  @RequestMapping(value = "/requisitions/periodsForInitiate", method = GET)
  public ResponseEntity<?> getProcessingPeriodIds(@RequestParam(value = "programId") UUID program,
                                            @RequestParam(value = "facilityId") UUID facility,
                                            @RequestParam(value = "emergency") boolean emergency) {
    if (null == facility || null == program) {
      throw new ValidationMessageException(
          new Message("requisition.error.periods-for-initiate.missing-parameters"));
    }

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facility, program);

    Collection<ProcessingPeriodDto> periods = periodService.getPeriods(
        program, facility, emergency
    );
    return new ResponseEntity<>(periods, HttpStatus.OK);
  }

  /**
   * Submits earlier initiated requisition.
   */
  @RequestMapping(value = "/requisitions/{id}/submit", method = RequestMethod.POST)
  public ResponseEntity<?> submitRequisition(@PathVariable("id") UUID requisitionId) {
    permissionService.canSubmitRequisition(requisitionId);
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    }

    BindingResult bindingResult = new BeanPropertyBindingResult(requisition, REQUISITION);
    validator.validate(requisition, bindingResult);

    if (bindingResult.hasErrors()) {
      LOGGER.warn("Validation for requisition failed: {}", getErrors(bindingResult));
      return new ResponseEntity<>(getErrors(bindingResult), HttpStatus.BAD_REQUEST);
    }
    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(requisition.getFacilityId(),
        requisition.getProgramId());

    LOGGER.debug("Submitting a requisition with id " + requisition.getId());

    Collection<OrderableProductDto> orderableProducts
        = orderableProductReferenceDataService.findAll();

    calculatePacksToShipForEachLineItem(requisition, orderableProducts);

    requisition.submit();
    
    saveStatusMessage(requisition);
    
    requisitionRepository.save(requisition);
    LOGGER.debug("Requisition with id " + requisition.getId() + " submitted");

    return new ResponseEntity<>(
        requisitionDtoBuilder.build(requisition), HttpStatus.OK
    );
  }

  /**
   * Deletes requisition with the given id.
   */
  @RequestMapping(value = "/requisitions/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteRequisition(@PathVariable("id") UUID requisitionId) {
    permissionService.canDeleteRequisition(requisitionId);
    requisitionService.delete(requisitionId);
    return new ResponseEntity<>(HttpStatus.NO_CONTENT);
  }

  /**
   * Allows updating requisitions.
   *
   * @param requisitionDto A requisitionDto bound to the request body
   * @param requisitionId  UUID of requisition which we want to update
   * @return ResponseEntity containing the updated requisition
   */
  @RequestMapping(value = "/requisitions/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateRequisition(@RequestBody RequisitionDto requisitionDto,
                                             @PathVariable("id") UUID requisitionId) {
    permissionService.canUpdateRequisition(requisitionId);

    Requisition requisitionToUpdate = requisitionRepository.findOne(requisitionId);

    if (requisitionToUpdate == null) {
      throw new ContentNotFoundMessageException(new Message(
          "requisition.error.requisition-not-found", requisitionId));
    }
    Requisition requisition = RequisitionBuilder.newRequisition(requisitionDto,
        requisitionToUpdate.getTemplate());

    if (requisition.getId() == null) {
      requisition.setId(requisitionId);
    } else if (!requisitionId.equals(requisition.getId())) {
      throw new ValidationMessageException(new Message("requisition.error.id-mismatch"));
    }


    RequisitionStatus status = requisitionToUpdate.getStatus();
    if (status != RequisitionStatus.APPROVED
        && status != RequisitionStatus.SKIPPED
        && status != RequisitionStatus.RELEASED) {
      LOGGER.debug("Updating requisition with id: " + requisitionId);

      BindingResult bindingResult = new BeanPropertyBindingResult(requisition, REQUISITION);
      draftValidator.validate(requisition, bindingResult);

      if (bindingResult.hasErrors()) {
        LOGGER.warn("Validation for requisition failed: {}", getErrors(bindingResult));
        return new ResponseEntity<>(getErrors(bindingResult), HttpStatus.BAD_REQUEST);
      }

      requisitionToUpdate.updateFrom(requisition,
          stockAdjustmentReasonReferenceDataService.getStockAdjustmentReasonsByProgram(
              requisition.getProgramId()));

      requisitionToUpdate = requisitionRepository.save(requisitionToUpdate);

      LOGGER.debug("Saved requisition with id: " + requisitionToUpdate.getId());
      return new ResponseEntity<>(
          requisitionDtoBuilder.build(requisitionToUpdate), HttpStatus.OK
      );
    } else {
      throw new ValidationMessageException(new Message(
          "requisition.error.update.can-not-update-with-status", requisition.getStatus()));
    }
  }

  /**
   * Get chosen requisition.
   *
   * @param requisitionId UUID of requisition whose we want to get
   * @return Requisition.
   */
  @RequestMapping(value = "/requisitions/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getRequisition(@PathVariable("id") UUID requisitionId) {
    permissionService.canViewRequisition(requisitionId);
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(requisitionDtoBuilder.build(requisition), HttpStatus.OK);
    }
  }

  /**
   * Finds requisitions matching all of provided parameters.
   */
  @RequestMapping(value = "/requisitions/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchRequisitions(
      @RequestParam(value = "facility", required = false) UUID facility,
      @RequestParam(value = "program", required = false) UUID program,
      @RequestParam(value = "createdDateFrom", required = false)
      @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime createdDateFrom,
      @RequestParam(value = "createdDateTo", required = false)
      @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime createdDateTo,
      @RequestParam(value = "processingPeriod", required = false)
          UUID processingPeriod,
      @RequestParam(value = "supervisoryNode", required = false) UUID supervisoryNode,
      @RequestParam(value = "requisitionStatus", required = false)
          RequisitionStatus[] requisitionStatuses,
      @RequestParam(value = "emergency", required = false) Boolean emergency) {
    List<Requisition> result = requisitionService.searchRequisitions(facility, program,
        createdDateFrom, createdDateTo, processingPeriod, supervisoryNode, requisitionStatuses,
        emergency);

    return new ResponseEntity<>(requisitionDtoBuilder.build(result), HttpStatus.OK);
  }

  /**
   * Skipping chosen requisition period.
   */
  @RequestMapping(value = "/requisitions/{id}/skip", method = RequestMethod.PUT)
  public ResponseEntity<RequisitionDto> skipRequisition(@PathVariable("id") UUID requisitionId) {
    permissionService.canUpdateRequisition(requisitionId);

    Requisition requisition = requisitionService.skip(requisitionId);
    return new ResponseEntity<>(requisitionDtoBuilder.build(requisition), HttpStatus.OK);
  }

  /**
   * Rejecting requisition which is waiting for approve.
   */
  @RequestMapping(value = "/requisitions/{id}/reject", method = RequestMethod.PUT)
  public ResponseEntity<RequisitionDto> rejectRequisition(@PathVariable("id") UUID id) {
    Requisition rejectedRequisition = requisitionService.reject(id);
    return new ResponseEntity<>(requisitionDtoBuilder.build(rejectedRequisition), HttpStatus.OK);
  }

  /**
   * Approve specified by id requisition.
   */
  @RequestMapping(value = "/requisitions/{id}/approve", method = RequestMethod.POST)
  public ResponseEntity<?> approveRequisition(@PathVariable("id") UUID requisitionId) {
    permissionService.canApproveRequisition(requisitionId);
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    }

    BindingResult bindingResult = new BeanPropertyBindingResult(requisition, REQUISITION);
    validator.validate(requisition, bindingResult);

    if (bindingResult.hasErrors()) {
      LOGGER.warn("Validation for requisition failed: {}", getErrors(bindingResult));
      return new ResponseEntity<>(getErrors(bindingResult), HttpStatus.BAD_REQUEST);
    }
    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(requisition.getFacilityId(),
        requisition.getProgramId());

    if (requisition.getStatus() == RequisitionStatus.AUTHORIZED
        || (configurationSettingService.getBoolValue("skipAuthorization")
        && requisition.getStatus() == RequisitionStatus.SUBMITTED)) {

      Collection<OrderableProductDto> orderableProducts
          = orderableProductReferenceDataService.findAll();

      calculatePacksToShipForEachLineItem(requisition, orderableProducts);

      requisition.approve();

      saveStatusMessage(requisition);

      requisitionRepository.save(requisition);

      LOGGER.debug("Requisition with id " + requisitionId + " approved");
      return new ResponseEntity<>(requisitionDtoBuilder.build(requisition), HttpStatus.OK);
    } else {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get requisitions to approve for right supervisor.
   */
  @RequestMapping(value = "/requisitions/requisitionsForApproval", method = RequestMethod.GET)
  public ResponseEntity<Collection<RequisitionDto>> listForApproval() {
    UserDto user = authenticationHelper.getCurrentUser();
    Set<Requisition> approvalRequisitions = requisitionService
        .getRequisitionsForApproval(user.getId());
    return new ResponseEntity<>(requisitionDtoBuilder.build(approvalRequisitions), HttpStatus.OK);
  }

  /**
   * Get all submitted Requisitions.
   *
   * @return Submitted requisitions.
   */
  @RequestMapping(value = "/requisitions/submitted", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getSubmittedRequisitions() {
    List<Requisition> submittedRequisitions = requisitionService.searchRequisitions(
        null, null, null, null, null, null,
        new RequisitionStatus[]{RequisitionStatus.SUBMITTED}, null);
    if (submittedRequisitions == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(
          requisitionDtoBuilder.build(submittedRequisitions), HttpStatus.OK);
    }
  }

  /**
   * Authorize given requisition.
   *
   * @param requisitionId UUID of Requisition to authorize.
   * @return ResponseEntity with authorized Requisition if authorization was successful.
   */
  @RequestMapping(value = "/requisitions/{id}/authorize", method = RequestMethod.POST)
  public ResponseEntity<?> authorizeRequisition(@PathVariable("id") UUID requisitionId) {
    permissionService.canAuthorizeRequisition(requisitionId);

    if (configurationSettingService.getBoolValue("skipAuthorization")) {
      throw new ValidationMessageException(
          new Message("requisition.error.authorization-to-be-skipped"));
    }

    Requisition requisition = requisitionRepository.findOne(requisitionId);

    if (requisition == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    }

    BindingResult bindingResult = new BeanPropertyBindingResult(requisition, REQUISITION);
    validator.validate(requisition, bindingResult);

    if (bindingResult.hasErrors()) {
      return new ResponseEntity<>(getErrors(bindingResult), HttpStatus.BAD_REQUEST);
    }
    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(requisition.getFacilityId(),
        requisition.getProgramId());

    Collection<OrderableProductDto> orderableProducts
        = orderableProductReferenceDataService.findAll();

    calculatePacksToShipForEachLineItem(requisition, orderableProducts);

    requisition.authorize();
    nullDataValuesOfRequisitionLineItems(requisition.getSkippedRequisitionLineItems());

    UUID supervisoryNode = supervisoryNodeReferenceDataService.findSupervisoryNode(
        requisition.getProgramId(), requisition.getFacilityId()).getId();
    requisition.setSupervisoryNodeId(supervisoryNode);

    saveStatusMessage(requisition);

    requisitionRepository.save(requisition);
    LOGGER.debug("Requisition: " + requisitionId + " authorized.");

    return new ResponseEntity<>(requisitionDtoBuilder.build(requisition), HttpStatus.OK);
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
   * @return ResponseEntity with list of approved requisitions.
   */
  @RequestMapping(value = "/requisitions/requisitionsForConvert", method = RequestMethod.GET)
  public ResponseEntity<?> listForConvertToOrder(
      @RequestParam(required = false) String filterValue,
      @RequestParam(required = false) String filterBy,
      @RequestParam(required = false, defaultValue = "programName") String sortBy,
      @RequestParam(required = false, defaultValue = "true") boolean descending,
      @RequestParam(required = false) Integer pageNumber,
      @RequestParam(required = false) Integer pageSize) {
    UserDto user = authenticationHelper.getCurrentUser();

    Collection<UUID> userManagedFacilities = fulfillmentFacilitiesReferenceDataService
        .getFulfillmentFacilities(user.getId())
        .stream().map(FacilityDto::getId).collect(Collectors.toList());

    Collection<RequisitionDto> approvedRequisitionList =
        requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
            filterValue, filterBy, sortBy, descending, pageNumber, pageSize);

    List<RequisitionWithSupplyingDepotsDto> response = new ArrayList<>();
    for (RequisitionDto requisition : approvedRequisitionList) {
      List<FacilityDto> facilities = requisitionService
          .getAvailableSupplyingDepots(requisition.getId()).stream()
          .filter(f -> userManagedFacilities.contains(f.getId())).collect(Collectors.toList());

      if (!facilities.isEmpty()) {
        response.add(new RequisitionWithSupplyingDepotsDto(requisition, facilities));
      }
    }

    return new ResponseEntity<>(response, HttpStatus.OK);
  }

  /**
   * Converting Requisition list to orders.
   *
   * @param list List of Requisitions with their supplyingDepots that will be converted to Orders
   * @return ResponseEntity with the "#200 OK" HTTP response status on success
   */
  @RequestMapping(value = "/requisitions/convertToOrder", method = RequestMethod.POST)
  public ResponseEntity<?> convertToOrder(@RequestBody List<ConvertToOrderDto> list) {
    UserDto user = authenticationHelper.getCurrentUser();
    permissionService.canConvertToOrder(list);
    requisitionService.convertToOrder(list, user);
    return new ResponseEntity<>(HttpStatus.CREATED);
  }

  /**
   * Print out requisition as a PDF file.
   *
   * @param id The UUID of the requisition to print
   * @return ResponseEntity with the "#200 OK" HTTP response status and PDF file on success, or
   *     ResponseEntity containing the error description status.
   */
  @RequestMapping(value = "/requisitions/{id}/print", method = RequestMethod.GET)
  @ResponseBody
  public ModelAndView print(HttpServletRequest request, @PathVariable("id") UUID id)
      throws JasperReportViewException {
    permissionService.canViewRequisition(id);

    Requisition requisition = requisitionRepository.findOne(id);
    if (requisition == null) {
      throw new ContentNotFoundMessageException(new Message(
          "requisition.error.requisition-not-found", id));
    }

    Template template = templateService.getByName(REQUISITION_PRINT_TEMPLATE_NAME);
    List<RequisitionLineItem> fullSupply = requisitionService.getFullSupplyItems(id)
        .stream().filter(l -> !l.getSkipped()).collect(Collectors.toList());
    List<RequisitionLineItem> nonFullSupply = requisitionService.getNonFullSupplyItems(id)
        .stream().filter(l -> !l.getSkipped()).collect(Collectors.toList());

    RequisitionReportDto reportDto = new RequisitionReportDto(
        requisitionDtoBuilder.build(requisition),
        requisitionExportHelper.exportToDtos(fullSupply),
        requisitionExportHelper.exportToDtos(nonFullSupply)
    );

    Map<String, Object> params = ReportUtils.createParametersMap();
    params.put("datasource", Collections.singletonList(reportDto));
    params.put("template", requisition.getTemplate());

    JasperReportsMultiFormatView jasperView =
        jasperReportsViewService.getJasperReportsView(template, request);
    return new ModelAndView(jasperView, params);
  }

  @InitBinder("requisition")
  protected void initBinder(final WebDataBinder binder) {
    binder.addValidators(validator);
  }

  private void calculatePacksToShipForEachLineItem(Requisition requisition,
                                               Collection<OrderableProductDto> orderableProducts) {
    requisition.getNonSkippedRequisitionLineItems().forEach(
        line -> line.setPacksToShip(getPacksToShip(orderableProducts, line)));
  }

  private Long getPacksToShip(Collection<OrderableProductDto> orderableProducts,
                              RequisitionLineItem line) {
    return orderableProducts.stream()
        .filter(product -> isProductEquals(line, product))
        .map(product -> calculatePacksToShip(line, product))
        .findFirst()
        .orElse(null);
  }

  private boolean isProductEquals(RequisitionLineItem line, OrderableProductDto product) {
    return product.getId().equals(line.getOrderableProductId());
  }

  private Long calculatePacksToShip(RequisitionLineItem line, OrderableProductDto product) {
    if (line.getOrderQuantity() != null) {
      return product.packsToOrder(line.getOrderQuantity());
    } else {
      return null;
    }
  }

  private void nullDataValuesOfRequisitionLineItems(
      List<RequisitionLineItem> requisitionLineItems) {
    for (RequisitionLineItem requisitionLineItem : requisitionLineItems) {
      requisitionLineItem.setBeginningBalance(null);
      requisitionLineItem.setTotalReceivedQuantity(null);
      requisitionLineItem.setTotalLossesAndAdjustments((Integer) null);
      requisitionLineItem.setStockOnHand((Integer) null);
      requisitionLineItem.setRequestedQuantityExplanation(null);
      requisitionLineItem.setRemarks(null);
      requisitionLineItem.setApprovedQuantity(null);
      requisitionLineItem.setRequestedQuantity(null);
      requisitionLineItem.setTotalConsumedQuantity((Integer) null);
      requisitionLineItem.setTotal((Integer) null);
      requisitionLineItem.setRequestedQuantityExplanation(null);
      requisitionLineItem.setTotalStockoutDays(null);
      requisitionLineItem.setPacksToShip(null);
      requisitionLineItem.setPricePerPack(null);
      requisitionLineItem.setTotalCost(null);
      requisitionLineItem.setNumberOfNewPatientsAdded(null);
      requisitionLineItem.setAdjustedConsumption(null);
      requisitionLineItem.setAverageConsumption(null);
      requisitionLineItem.setMaximumStockQuantity((Integer) null);
      requisitionLineItem.setCalculatedOrderQuantity((Integer) null);
      requisitionLineItem.clearStockAdjustmentsAndPreviousAdjustedConsumptions();
    }
  }
  
  private void saveStatusMessage(Requisition requisition) {
    if (requisition.getDraftStatusMessage() != null) {
      StatusMessage newStatusMessage = StatusMessage.newStatusMessage(requisition,
          authenticationHelper.getCurrentUser().getId(), requisition.getStatus(),
          requisition.getDraftStatusMessage());
      statusMessageRepository.save(newStatusMessage);
      requisition.setDraftStatusMessage(null);
    }
  }
}
