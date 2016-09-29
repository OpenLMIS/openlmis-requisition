package org.openlmis.reporting.web;

import org.apache.log4j.Logger;
import org.openlmis.reporting.exception.ReportingException;
import org.openlmis.reporting.model.Template;
import org.openlmis.reporting.repository.TemplateRepository;
import org.openlmis.reporting.service.TemplateService;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.web.BaseController;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.multipart.MultipartFile;

import java.util.UUID;

@Controller
public class TemplateController extends BaseController {

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
   */
  @RequestMapping(value = "/templates", method = RequestMethod.POST)
  @ResponseStatus(HttpStatus.OK)
  public void createJasperReportTemplate(@RequestPart("file") MultipartFile file,
                                                      String name, String description)
          throws ReportingException {
    Template template = new Template(name, null, null, CONSISTENCY_REPORT, description);
    templateService.validateFileAndInsertTemplate(template, file);
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
    return new ResponseEntity<>(templates, HttpStatus.OK);
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

    Template templateToUpdate = templateRepository.findOne(templateId);
    if (templateToUpdate == null) {
      templateToUpdate = new Template();
      LOGGER.info("Creating new template");
    } else {
      LOGGER.debug("Updating template with id: " + templateId);
    }

    templateToUpdate.updateFrom(template);
    templateToUpdate = templateRepository.save(templateToUpdate);

    LOGGER.debug("Saved template with id: " + templateToUpdate.getId());
    return new ResponseEntity<>(templateToUpdate, HttpStatus.OK);
  }

  /**
   * Get chosen template.
   *
   * @param templateId UUID of template which we want to get
   * @return Template.
   */
  @RequestMapping(value = "/templates/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getTemplate(@PathVariable("id") UUID templateId) {
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
      templateRepository.delete(template);
      return new ResponseEntity<RequisitionTemplate>(HttpStatus.NO_CONTENT);
    }
  }
}
