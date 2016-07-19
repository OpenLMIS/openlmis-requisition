package org.openlmis.requisition.web;

import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.validate.RequisitionValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.HttpStatus.CREATED;
import static org.springframework.web.bind.annotation.RequestMethod.POST;

@RepositoryRestController
public class RequisitionController {
  Logger logger = LoggerFactory.getLogger(RequisitionController.class);

  @Autowired
  RequisitionRepository requisitionRepository;

  @Autowired
  @Qualifier("beforeSaveRequisitionValidator")
  RequisitionValidator validator;

  @Autowired
  RequisitionService requisitionService;

  @RequestMapping(value = "/requisitions", method = POST)
  public ResponseEntity<?> initiateRnr(@RequestParam("facilityId") UUID facilityId,
                                       @RequestParam("programId") UUID programId,
                                       @RequestParam("periodId") UUID periodId,
                                       @RequestParam("emergency") Boolean emergency) {
    try {

      Requisition requisition = requisitionService.initiateRequisition(
          facilityId, programId, periodId, emergency);
      ResponseEntity response = new ResponseEntity<>(requisition, CREATED);
      return response;

    } catch (RequisitionException ex) {
      return new ResponseEntity(BAD_REQUEST);
    }
  }

  /**
   * Submits earlier initiated requisition.
   */
  @RequestMapping(value = "/requisitions/submit", method = RequestMethod.POST)
  public ResponseEntity<?> submitRequisition(@RequestBody Requisition requisition,
                                             BindingResult bindingResult) {
    if (requisition == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      validator.validate(requisition, bindingResult);

      if (bindingResult.getErrorCount() == 0) {
        logger.debug("Submitting a requisition");
        requisition.setStatus(RequisitionStatus.SUBMITTED);
        Requisition newRequisition = requisitionRepository.save(requisition);
        return new ResponseEntity<>(newRequisition, HttpStatus.CREATED);
      } else {
        return new ResponseEntity<>(getRequisitionErrors(bindingResult), HttpStatus.BAD_REQUEST);
      }
    }
  }

  @RequestMapping(value = "/requisitions/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteRequisition(@PathVariable("id") UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    boolean deleted = requisitionService.tryDelete(requisition);

    if (deleted) {
      return new ResponseEntity(HttpStatus.NO_CONTENT);
    } else {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Searching requisitions created by logged user.
   */
  @RequestMapping(value = "/requisitions/creator/{creatorId}", method = RequestMethod.GET)
  public ResponseEntity<?> createdByLoggedUser(@PathVariable("creatorId") UUID id) {
    Iterable<Requisition> result = requisitionRepository.findByCreatorId(id);
    return new ResponseEntity<>(result, HttpStatus.OK);
  }

  /**
   * Finds requisitions matching all of provided parameters.
   */
  @RequestMapping(value = "/requisitions/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchRequisitions(
      @RequestParam(value = "facility", required = false) Facility facility,
      @RequestParam(value = "program", required = false) Program program,
      @RequestParam(value = "createdDateFrom", required = false)
      @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime createdDateFrom,
      @RequestParam(value = "createdDateTo", required = false)
      @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime createdDateTo) {

    List<Requisition> result = requisitionService.searchRequisitions(facility, program,
        createdDateFrom, createdDateTo);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }

  /**
   * Skipping chosen requisition period.
   */
  @RequestMapping(value = "/requisitions/{id}/skip", method = RequestMethod.PUT)
  public ResponseEntity<?> skipRequisition(@PathVariable("id") UUID requisitionId) {
    boolean skipped = requisitionService.skip(requisitionId);
    ResponseEntity<Object> responseEntity;
    if (skipped) {
      Requisition requisition = requisitionRepository.findOne(requisitionId);
      responseEntity = new ResponseEntity<Object>(requisition, HttpStatus.OK);
    } else {
      responseEntity = new ResponseEntity<Object>(HttpStatus.BAD_REQUEST);
    }
    return responseEntity;
  }

  /**
   * Rejecting requisition which is waiting for approve.
   */
  @RequestMapping(value = "/requisitions/{id}/reject", method = RequestMethod.PUT)
  public ResponseEntity<?> rejectRequisition(@PathVariable("id") UUID id) {

    try {
      requisitionService.reject(id);
    } catch (RequisitionException ex) {
      logger.debug(ex.getMessage(), ex);
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }
    Requisition rejectedRequisition = requisitionRepository.findOne(id);
    return new ResponseEntity<>(rejectedRequisition, HttpStatus.OK);
  }

  private Map<String, String> getRequisitionErrors(BindingResult bindingResult) {
    return new HashMap<String, String>() {
      {
        for (FieldError error : bindingResult.getFieldErrors()) {
          put(error.getField(), error.getDefaultMessage());
        }
      }
    };
  }
}