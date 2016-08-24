package org.openlmis.reporting.web;

import org.apache.log4j.Logger;
import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
import org.openlmis.reporting.exception.ReportingException;
import org.openlmis.reporting.model.Template;
import org.openlmis.reporting.repository.TemplateRepository;
import org.openlmis.reporting.service.TemplateService;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.multipart.MultipartFile;

import java.util.UUID;

@RepositoryRestController
public class TemplateController {

  private static final String CONSISTENCY_REPORT = "Consistency Report";

  private static final Logger LOGGER = Logger.getLogger(TemplateController.class);

  @Autowired
  private TemplateService templateService;

  @Autowired
  private TemplateRepository templateRepository;

  /**
   * Adding report templates with ".jrxml" format to database.
   *
   * @param file        File in ".jrxml" format to upload
   * @param name        Name of file in database
   * @param description Description of the file
   * @return ResponseEntity with the "#200 OK" HTTP response status on success
   *         or ResponseEntity containing the error description status.
   */
  @RequestMapping(value = "/templates", method = RequestMethod.POST)
  public ResponseEntity<?> createJasperReportTemplate(@RequestPart("file") MultipartFile file,
                                                      String name, String description) {
    Template template = new Template(name, null, null, CONSISTENCY_REPORT, description);
    try {
      templateService.validateFileAndInsertTemplate(template, file);
    } catch (ReportingException ex) {
      LOGGER.debug(ex);
      return new ResponseEntity<>(ex.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
    }
    return new ResponseEntity<>(HttpStatus.OK);
  }

  /**
   * Get all templates.
   *
   * @return Templates.
   */
  @RequestMapping(value = "/templates", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllTemplates() {
    Iterable<Template> templates = templateRepository.findAll();
    if (templates == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(templates, HttpStatus.OK);
    }
  }

  /**
   * Allows updating templates.
   *
   * @param template A template bound to the request body
   * @param templateId UUID of template which we want to update
   * @return ResponseEntity containing the updated template
   */
  @RequestMapping(value = "/templates/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateTemplate(@RequestBody Template template,
                                          @PathVariable("id") UUID templateId) {
    Template templateFromDb =
          templateRepository.findOne(templateId);
    if (templateFromDb == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      LOGGER.debug("Updating template");
      Template updatedTemplate =
            templateRepository.save(template);
      return new ResponseEntity<Template>(updatedTemplate, HttpStatus.OK);
    }
  }

  /**
   * Get chosen template.
   *
   * @param templateId UUID of template which we want to get
   * @return Template.
   */
  @RequestMapping(value = "/templates/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> geTemplate(@PathVariable("id") UUID templateId) {
    Template template =
          templateRepository.findOne(templateId);
    if (template == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(template, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting template.
   *
   * @param templateId UUID of template which we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/templates/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteTemplate(@PathVariable("id")
                                                           UUID templateId) {
    Template template =
          templateRepository.findOne(templateId);
    if (template == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        templateRepository.delete(template);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("Template cannot be deleted because of existing dependencies",
                    ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<RequisitionTemplate>(HttpStatus.NO_CONTENT);
    }
  }
}
