package org.openlmis.requisition.web;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.InvalidRequisitionStatusException;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.requisition.validate.RequisitionValidator;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.ErrorResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

import javax.validation.Valid;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.springframework.web.bind.annotation.RequestMethod.POST;

@SuppressWarnings("PMD.TooManyMethods")
@Controller
public class RequisitionController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionController.class);

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  @Qualifier("beforeSaveRequisitionValidator")
  private RequisitionValidator validator;

  @Autowired
  private RequisitionService requisitionService;

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  @InitBinder("requisition")
  protected void initBinder(final WebDataBinder binder) {
    binder.addValidators(validator);
  }

  /**
   * Allows creating new requisitions.
   *
   * @param requisition A requisition bound to the request body
   * @return ResponseEntity containing the created requisition
   */
  @RequestMapping(value = "/requisitions/initiate", method = POST)
  public ResponseEntity<?> initiateRequisition(@RequestBody @Valid Requisition requisition,
                                               BindingResult bindingResult) {
    try {
      Requisition newRequisition = requisitionService.initiateRequisition(requisition);
      return new ResponseEntity<>(newRequisition, HttpStatus.CREATED);
    } catch (RequisitionException ex) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Submits earlier initiated requisition.
   */
  @RequestMapping(value = "/requisitions/{id}/submit", method = RequestMethod.PUT)
  public ResponseEntity<?> submitRequisition(@RequestBody @Valid Requisition requisition,
                                             BindingResult bindingResult,
                                             @PathVariable("id") UUID requisitionId) {
    if (bindingResult.hasErrors()) {
      return new ResponseEntity<>(getRequisitionErrors(bindingResult), HttpStatus.BAD_REQUEST);
    }

    Requisition savedRequisition = requisitionRepository.findOne(requisitionId);

    if (savedRequisition == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    }

    try {
      LOGGER.debug("Submitting a requisition with id " + requisition.getId());
      requisition.submit();
      requisitionRepository.save(requisition);
      LOGGER.debug("Requisition with id " + requisition.getId() + " submitted");
    } catch (RequisitionException ex) {
      ErrorResponse errorResponse =
              new ErrorResponse("An error occurred while submitting requisition with id: "
                      + requisition.getId(), ex.getMessage());
      LOGGER.debug(errorResponse.getMessage(), ex);
      return new ResponseEntity<>(errorResponse, HttpStatus.BAD_REQUEST);
    }
    return new ResponseEntity<Object>(requisition, HttpStatus.OK);
  }

  /**
   * Deletes requisition with the given id.
   */
  @RequestMapping(value = "/requisitions/{id}", method = RequestMethod.DELETE)
  @ResponseStatus(HttpStatus.NO_CONTENT)
  public void deleteRequisition(@PathVariable("id") UUID requisitionId)
          throws RequisitionException {
    requisitionService.delete(requisitionId);
  }

  /**
   * Allows updating requisitions.
   *
   * @param requisition A requisition bound to the request body
   * @param requisitionId UUID of requisition which we want to update
   * @return ResponseEntity containing the updated requisition
   */
  @RequestMapping(value = "/requisitions/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateRequisition(@RequestBody Requisition requisition,
                                       @PathVariable("id") UUID requisitionId)
          throws InvalidRequisitionStatusException {

    Requisition requisitionToUpdate = requisitionRepository.findOne(requisitionId);
    if (requisitionToUpdate.getStatus() == RequisitionStatus.INITIATED) {
      LOGGER.debug("Updating requisition with id: " + requisitionId);
      requisitionToUpdate.updateFrom(requisition);
      requisitionToUpdate = requisitionRepository.save(requisitionToUpdate);

      LOGGER.debug("Saved requisition with id: " + requisitionToUpdate.getId());
      return new ResponseEntity<>(requisitionToUpdate, HttpStatus.OK);
    } else {
      throw new InvalidRequisitionStatusException("Cannot update a requisition "
              + "with status: " + requisition.getStatus());
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
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(requisition, HttpStatus.OK);
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
              RequisitionStatus requisitionStatus) {

    List<Requisition> result = requisitionService.searchRequisitions(facility, program,
        createdDateFrom, createdDateTo, processingPeriod, supervisoryNode, requisitionStatus);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }

  /**
   * Skipping chosen requisition period.
   */
  @RequestMapping(value = "/requisitions/{id}/skip", method = RequestMethod.PUT)
  public ResponseEntity<?> skipRequisition(@PathVariable("id") UUID requisitionId)
          throws RequisitionException {
    Requisition requisition = requisitionService.skip(requisitionId);
    return new ResponseEntity<>(requisition, HttpStatus.OK);
  }

  /**
   * Rejecting requisition which is waiting for approve.
   */
  @RequestMapping(value = "/requisitions/{id}/reject", method = RequestMethod.PUT)
  public ResponseEntity<?> rejectRequisition(@PathVariable("id") UUID id)
          throws RequisitionException {
    Requisition rejectedRequisition = requisitionService.reject(id);
    return new ResponseEntity<>(rejectedRequisition, HttpStatus.OK);
  }

  /**
   * Approve specified by id requisition.
   */
  @RequestMapping(value = "/requisitions/{id}/approve", method = RequestMethod.PUT)
  public ResponseEntity<?> approveRequisition(@PathVariable("id") UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    }
    if (requisition.getStatus() == RequisitionStatus.AUTHORIZED
        || (configurationSettingService.getBoolValue("skipAuthorization")
        && requisition.getStatus() == RequisitionStatus.SUBMITTED)) {
      requisition.setStatus(RequisitionStatus.APPROVED);
      requisitionRepository.save(requisition);
      LOGGER.debug("Requisition with id " + requisitionId + " approved");
      return new ResponseEntity<>(requisition, HttpStatus.OK);
    } else {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get requisitions to approve for right supervisor.
   */
  @RequestMapping(value = "/requisitions/requisitions-for-approval", method = RequestMethod.GET)
  public ResponseEntity<Object> listForApproval() {
    String userName =
        (String) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
    Map<String, Object> parameters = new HashMap<>();
    parameters.put("username", userName);
    List<UserDto> users = new ArrayList<>(userReferenceDataService.findAll("search", parameters));
    List<Requisition> requisitions =
        requisitionService.getRequisitionsForApproval(users.get(0).getId());
    return new ResponseEntity<>(requisitions, HttpStatus.OK);
  }

  /**
   * Get all submitted Requisitions.
   *
   * @return Submitted requisitions.
   */
  @RequestMapping(value = "/requisitions/submitted", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getSubmittedRequisitions() {

    Iterable<Requisition> submittedRequisitions = requisitionService.searchRequisitions(
                null, null, null, null, null, null, RequisitionStatus.SUBMITTED);
    if (submittedRequisitions == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(submittedRequisitions, HttpStatus.OK);
    }
  }

  /**
   * Authorize given requisition.
   *
   * @param requisition Requisition object to be authorized.
   * @param bindingResult Object used for validation.
   * @param requisitionId UUID of Requisition to authorize.
   * @return ResponseEntity with authorized Requisition if authorization was successful.
   */
  @RequestMapping(value = "/requisitions/{id}/authorize", method = RequestMethod.PUT)
  public ResponseEntity<?> authorizeRequisition(@RequestBody @Valid Requisition requisition,
                                                BindingResult bindingResult,
                                                @PathVariable("id") UUID requisitionId)
          throws RequisitionException {

    if (configurationSettingService.getBoolValue("skipAuthorization")) {
      return new ResponseEntity<>("Requisition authorization is configured to be skipped",
          HttpStatus.BAD_REQUEST);
    }

    if (bindingResult.hasErrors()) {
      return new ResponseEntity<>(getRequisitionErrors(bindingResult), HttpStatus.BAD_REQUEST);
    }

    Requisition savedRequisition = requisitionRepository.findOne(requisitionId);

    if (savedRequisition == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    }

    requisition.authorize();
    requisitionRepository.save(requisition);
    LOGGER.debug("Requisition: " +  requisitionId + " authorized.");

    return new ResponseEntity<>(requisition, HttpStatus.OK);
  }

  /**
   * Get approved requisitions matching all of provided parameters.
   *
   * @param filterValue Value to be used to filter.
   * @param filterBy Field used to filter: "programName", "facilityCode", "facilityName" or "all".
   * @param sortBy Field used to sort: "programName", "facilityCode" or "facilityName".
   * @param descending Descending direction for sort.
   * @param pageNumber Page number to return.
   * @param pageSize Quantity for one page.
   *
   * @return ResponseEntity with list of approved requisitions.
   */
  @RequestMapping(value = "/requisitions/approved/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchApprovedRequisitionsWithSortAndFilterAndPaging(
      @RequestParam String filterValue,
      @RequestParam String filterBy,
      @RequestParam String sortBy,
      @RequestParam Boolean descending,
      @RequestParam Integer pageNumber,
      @RequestParam Integer pageSize) {

    // TODO Add filtering about available Requisition for user
    // (If Reference Data Service - EBAC will be finished)
    // TODO Add available supplying depot and filtering about this
    // (If OLMIS-227 will be finished)

    List<Requisition> approvedRequisitionList =
        requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
            filterValue, filterBy, sortBy, descending, pageNumber, pageSize);

    return new ResponseEntity<>(approvedRequisitionList, HttpStatus.OK);
  }

  private Map<String, String> getRequisitionErrors(BindingResult bindingResult) {
    return new HashMap<String, String>() {
      {
        for (FieldError error : bindingResult.getFieldErrors()) {
          put(error.getField(), error.getCode());
        }
      }
    };
  }
}
