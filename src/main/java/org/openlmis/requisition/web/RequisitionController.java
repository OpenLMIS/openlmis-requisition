package org.openlmis.requisition.web;

import static org.springframework.web.bind.annotation.RequestMethod.POST;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.referencedata.domain.Comment;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.repository.CommentRepository;
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
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.oauth2.provider.OAuth2Authentication;
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

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.validation.Valid;

@RepositoryRestController
@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionController {
  Logger logger = LoggerFactory.getLogger(RequisitionController.class);

  @Autowired
  RequisitionRepository requisitionRepository;

  @Autowired
  @Qualifier("beforeSaveRequisitionValidator")
  RequisitionValidator validator;

  @Autowired
  RequisitionService requisitionService;

  @Autowired
  private CommentRepository commentRepository;

  @InitBinder("requisition")
  protected void initBinder(final WebDataBinder binder) {
    binder.addValidators(validator);
  }

  /**
   * Initiates requisition.
   * 
   * @param facilityId The UUID of the requisition's facility
   * @param programId The UUID of the requisition's program
   * @param periodId The UUID of the requisition's period
   * @param emergency Boolean indicating emergency status of requisition
   * @return result
   */
  @RequestMapping(value = "/requisitions", method = POST)
  public ResponseEntity<?> initiateRequisition(@RequestParam("facilityId") UUID facilityId,
                                       @RequestParam("programId") UUID programId,
                                       @RequestParam("periodId") UUID periodId,
                                       @RequestParam("emergency") Boolean emergency) {
    try {

      Requisition requisition = requisitionService.initiateRequisition(
          facilityId, programId, periodId, emergency);
      ResponseEntity response = new ResponseEntity<>(requisition, HttpStatus.CREATED);
      return response;

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
    if (!bindingResult.hasErrors()) {
      try {
        requisition = requisitionService.submitRequisition(requisitionId);
      } catch (RequisitionException ex) {
        logger.debug(ex.getMessage(), ex);
        return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
      }
      return new ResponseEntity<Object>(requisition, HttpStatus.OK);
    } else {
      return new ResponseEntity(getRequisitionErrors(bindingResult), HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Deletes requisition with the given id.
   */
  @RequestMapping(value = "/requisitions/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteRequisition(@PathVariable("id") UUID requisitionId) {
    boolean deleted = requisitionService.tryDelete(requisitionId);

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

  /**
   * Add comment to the requisition.
   */
  @PreAuthorize("isAuthenticated()")
  @RequestMapping(value = "/requisitions/{id}/comments", method = RequestMethod.POST)
  public ResponseEntity<Object> insertComment(@RequestBody Comment comment,
                                         @PathVariable("id") UUID id, OAuth2Authentication auth) {
    Requisition requisition = requisitionRepository.findOne(id);
    comment.setRequisition(requisition);

    User user = (User) auth.getPrincipal();

    comment.setAuthor(user);
    commentRepository.save(comment);

    List<Comment> comments = requisitionService.getCommentsByReqId(id);
    return new ResponseEntity<>(comments, HttpStatus.OK);
  }

  /**
   * Get all comments for specified requisition.
   */
  @RequestMapping(value = "/requisitions/{id}/comments", method = RequestMethod.GET)
  public ResponseEntity<Object> getCommentsForRequisition(@PathVariable("id") UUID id) {
    List<Comment> comments = requisitionService.getCommentsByReqId(id);
    return new ResponseEntity<Object>(comments, HttpStatus.OK);
  }

  /**
   * Approve specified by id requisition.
   */
  @PreAuthorize("isAuthenticated()")
  @RequestMapping(value = "/requisitions/{id}/approve", method = RequestMethod.PUT)
  public ResponseEntity<?> approveRequisition(@PathVariable("id") UUID requisitionId) {
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    }
    if (requisition.getStatus() == RequisitionStatus.AUTHORIZED) {
      requisition.setStatus(RequisitionStatus.APPROVED);
      requisitionRepository.save(requisition);
      logger.debug("Requisition with id " + requisitionId + " approved");
      return new ResponseEntity<>(requisition, HttpStatus.OK);
    } else {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get requisitions to approve for right supervisor.
   */
  @PreAuthorize("isAuthenticated()")
  @RequestMapping(value = "/requisitions-for-approval", method = RequestMethod.GET)
  public ResponseEntity<Object> listForApproval(OAuth2Authentication auth) {
    User user = (User) auth.getPrincipal();
    List<Requisition> requisitions = requisitionService.getRequisitionsForApproval(user.getId());
    return new ResponseEntity<Object>(requisitions, HttpStatus.OK);
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

  @RequestMapping(value = "/requisitions/submitted", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getSubmittedRequisitions() {

    Iterable<Requisition> submittedRequisitions =
        requisitionRepository.findByStatus(RequisitionStatus.SUBMITTED);
    if (submittedRequisitions == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(submittedRequisitions, HttpStatus.OK);
    }
  }

  @RequestMapping(value = "/requisitions/{id}/authorize", method = RequestMethod.PUT)
  public ResponseEntity<?> authorizeRequisition(@PathVariable("id") UUID requisitionId) {

    if (requisitionId == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    }

    Requisition requisition;
    try {
      requisition = requisitionService.authorize(requisitionId);
      logger.info("Requisition: " +  requisitionId + " authorize.");

    } catch (RequisitionException ex) {
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }
    return new ResponseEntity<>(requisition, HttpStatus.OK);

  }
}