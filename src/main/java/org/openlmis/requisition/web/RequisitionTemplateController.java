package org.openlmis.requisition.web;

import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionTemplateService;
import org.openlmis.requisition.validate.RequisitionTemplateValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.UUID;

import javax.validation.Valid;

@Controller
public class RequisitionTemplateController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionTemplateController.class);

  @Autowired
  private RequisitionTemplateService requisitionTemplateService;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private RequisitionTemplateValidator validator;

  @Autowired
  private PermissionService permissionService;

  @InitBinder
  protected void initBinder(final WebDataBinder binder) {
    binder.addValidators(validator);
  }

  /**
   * Allows creating new requisitionLineItems.
   * If the id is specified, it will be ignored.
   *
   * @param requisitionTemplate A requisitionTemplate bound to the request body
   * @param bindingResult       Object used for validation.
   * @return ResponseEntity containing the created requisitionTemplate
   */
  @RequestMapping(value = "/requisitionTemplates", method = RequestMethod.POST)
  public ResponseEntity<?> createRequisitionTemplate(
      @RequestBody @Valid RequisitionTemplate requisitionTemplate, BindingResult bindingResult) {
    permissionService.canManageRequisitionTemplate();
    if (bindingResult.hasErrors()) {
      return new ResponseEntity<>(getErrors(bindingResult), HttpStatus.BAD_REQUEST);
    }

    LOGGER.debug("Creating new requisitionTemplate");
    requisitionTemplate.setId(null);
    RequisitionTemplate newRequisitionTemplate =
        requisitionTemplateRepository.save(requisitionTemplate);
    LOGGER.debug("Created new requisitionTemplate with id: " + requisitionTemplate.getId());
    return new ResponseEntity<>(newRequisitionTemplate, HttpStatus.CREATED);
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
   * @param requisitionTemplate   A requisitionTemplate bound to the request body
   * @param requisitionTemplateId UUID of requisitionTemplate which we want to update
   * @param bindingResult         Object used for validation.
   * @return ResponseEntity containing the updated requisitionTemplate
   */
  @RequestMapping(value = "/requisitionTemplates/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateRequisitionTemplate(
      @RequestBody @Valid RequisitionTemplate requisitionTemplate,
      @PathVariable("id") UUID requisitionTemplateId, BindingResult bindingResult) {
    permissionService.canManageRequisitionTemplate();
    if (bindingResult.hasErrors()) {
      return new ResponseEntity<>(getErrors(bindingResult), HttpStatus.BAD_REQUEST);
    }

    RequisitionTemplate requisitionTemplateToUpdate =
        requisitionTemplateRepository.findOne(requisitionTemplateId);
    if (requisitionTemplateToUpdate == null) {
      requisitionTemplateToUpdate = new RequisitionTemplate();
      requisitionTemplateToUpdate.updateFrom(requisitionTemplate);
      LOGGER.info("Creating new requisitionTemplate");
    } else {
      requisitionTemplate.setId(requisitionTemplateToUpdate.getId());
      requisitionTemplateToUpdate = requisitionTemplate;
      LOGGER.debug("Updating requisitionTemplate with id: " + requisitionTemplateId);
    }

    requisitionTemplateToUpdate = requisitionTemplateService.save(requisitionTemplateToUpdate);

    LOGGER.debug("Saved requisitionTemplate with id: " + requisitionTemplateToUpdate.getId());
    return new ResponseEntity<>(requisitionTemplateToUpdate, HttpStatus.OK);
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
    permissionService.canManageRequisitionTemplate();
    RequisitionTemplate requisitionTemplate =
        requisitionTemplateRepository.findOne(requisitionTemplateId);
    if (requisitionTemplate == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      requisitionTemplateRepository.delete(requisitionTemplate);
      return new ResponseEntity<RequisitionTemplate>(HttpStatus.NO_CONTENT);
    }
  }

  /**
   * Returns requisition template for the given program.
   *
   * @param program program of searched requisition templates.
   * @return ResponseEntity with list of all requisition templates matching provided parameters and
   *     OK httpStatus.
   */
  @RequestMapping(value = "/requisitionTemplates/search", method = RequestMethod.GET)
  @ResponseBody
  public RequisitionTemplate getTemplateByProgram(
      @RequestParam(value = "program", required = false) UUID program) {
    return requisitionTemplateService.getTemplateForProgram(program);
  }
}
