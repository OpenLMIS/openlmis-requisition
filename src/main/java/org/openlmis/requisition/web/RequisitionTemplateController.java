package org.openlmis.requisition.web;

import org.openlmis.utils.ErrorResponse;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.service.RequisitionTemplateService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.List;
import java.util.UUID;

@Controller
public class RequisitionTemplateController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionTemplateController.class);

  @Autowired
  private RequisitionTemplateService requisitionTemplateService;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  /**
   * Allows creating new requisitionLines.
   * If the id is specified, it will be ignored.
   *
   * @param requisitionTemplate A requisitionTemplate bound to the request body
   * @return ResponseEntity containing the created requisitionTemplate
   */
  @RequestMapping(value = "/requisitionTemplates", method = RequestMethod.POST)
  public ResponseEntity<?> createRequisitionTemplate(
        @RequestBody RequisitionTemplate requisitionTemplate) {
    try {
      LOGGER.debug("Creating new requisitionTemplate");
      requisitionTemplate.setId(null);
      RequisitionTemplate newRequisitionTemplate =
            requisitionTemplateRepository.save(requisitionTemplate);
      LOGGER.debug("Created new requisitionTemplate with id: " + requisitionTemplate.getId());
      return new ResponseEntity<RequisitionTemplate>(newRequisitionTemplate, HttpStatus.CREATED);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while creating requisitionTemplate",
                  ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get all requisitionTemplates.
   *
   * @return RequisitionTemplates.
   */
  @RequestMapping(value = "/requisitionTemplates", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllRequisitionTemplates() {
    Iterable<RequisitionTemplate> requisitionTemplates = requisitionTemplateRepository.findAll();
    return new ResponseEntity<>(requisitionTemplates, HttpStatus.OK);
  }

  /**
   * Allows updating requisitionTemplates.
   *
   * @param requisitionTemplate A requisitionTemplate bound to the request body
   * @param requisitionTemplateId UUID of requisitionTemplate which we want to update
   * @return ResponseEntity containing the updated requisitionTemplate
   */
  @RequestMapping(value = "/requisitionTemplates/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateRequisitionTemplate(
        @RequestBody RequisitionTemplate requisitionTemplate,
        @PathVariable("id") UUID requisitionTemplateId) {

    RequisitionTemplate requisitionTemplateToUpdate =
          requisitionTemplateRepository.findOne(requisitionTemplateId);
    try {
      if (requisitionTemplateToUpdate == null) {
        requisitionTemplateToUpdate = new RequisitionTemplate();
        LOGGER.info("Creating new requisitionTemplate");
      } else {
        LOGGER.debug("Updating requisitionTemplate with id: " + requisitionTemplateId);
      }

      requisitionTemplateToUpdate.updateFrom(requisitionTemplate);
      requisitionTemplateToUpdate = requisitionTemplateRepository.save(requisitionTemplateToUpdate);

      LOGGER.debug("Saved requisitionTemplate with id: " + requisitionTemplateToUpdate.getId());
      return new ResponseEntity<RequisitionTemplate>(requisitionTemplateToUpdate, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while saving requisitionTemplate with id: "
                  + requisitionTemplateToUpdate.getId(), ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get chosen requisitionTemplate.
   *
   * @param requisitionTemplateId UUID of requisitionTemplate which we want to get
   * @return RequisitionTemplate.
   */
  @RequestMapping(value = "/requisitionTemplates/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getRequisitionTemplate(@PathVariable("id") UUID requisitionTemplateId) {
    RequisitionTemplate requisitionTemplate =
          requisitionTemplateRepository.findOne(requisitionTemplateId);
    if (requisitionTemplate == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(requisitionTemplate, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting requisitionTemplate.
   *
   * @param requisitionTemplateId UUID of requisitionTemplate which we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/requisitionTemplates/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteRequisitionTemplate(@PathVariable("id")
                                                             UUID requisitionTemplateId) {
    RequisitionTemplate requisitionTemplate =
          requisitionTemplateRepository.findOne(requisitionTemplateId);
    if (requisitionTemplate == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        requisitionTemplateRepository.delete(requisitionTemplate);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("An error accurred while deleting requisitionTemplate with id: "
                    + requisitionTemplateId, ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<RequisitionTemplate>(HttpStatus.NO_CONTENT);
    }
  }

  /**
   * Returns all requisition templates with matched parameters.
   * @param program program of searched requisition templates.
   * @return ResponseEntity with list of all requisition templates matching
   *         provided parameters and OK httpStatus.
   */
  @RequestMapping(value = "/requisitionTemplates/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchRequisitionTemplates(
      @RequestParam(value = "program", required = false) ProgramDto program) {
    List<RequisitionTemplate> result
        = requisitionTemplateService.searchRequisitionTemplates(program);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }
}
