package org.openlmis.requisition.web;

import org.apache.log4j.Logger;
import org.openlmis.requisition.domain.JasperTemplate;
import org.openlmis.requisition.dto.JasperTemplateDto;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.ReportingException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.repository.JasperTemplateRepository;
import org.openlmis.requisition.service.JasperTemplateService;
import org.openlmis.utils.Message;
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
@RequestMapping("/api/reports/templates/requisitions")
public class JasperTemplateController extends BaseController {

  private static final String CONSISTENCY_REPORT = "Consistency Report";

  private static final Logger LOGGER = Logger.getLogger(JasperTemplateController.class);

  @Autowired
  private JasperTemplateService jasperTemplateService;

  @Autowired
  private JasperTemplateRepository jasperTemplateRepository;

  /**
   * Adding report templates with ".jrxml" format to database.
   *
   * @param file        File in ".jrxml" format to upload
   * @param name        Name of file in database
   * @param description Description of the file
   */
  @RequestMapping(method = RequestMethod.POST)
  @ResponseStatus(HttpStatus.OK)
  public void createJasperReportTemplate(
      @RequestPart("file") MultipartFile file, String name, String description)
      throws ReportingException {
    JasperTemplate jasperTemplate = new JasperTemplate(
        name, null, null, CONSISTENCY_REPORT, description);
    jasperTemplateService.validateFileAndInsertTemplate(jasperTemplate, file);
  }

  /**
   * Get all templates.
   *
   * @return Templates.
   */
  @RequestMapping(method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<Iterable<JasperTemplateDto>> getAllTemplates() {
    Iterable<JasperTemplateDto> templates =
        JasperTemplateDto.newInstance(jasperTemplateRepository.findAll());
    return new ResponseEntity<>(templates, HttpStatus.OK);
  }

  /**
   * Allows updating templates.
   *
   * @param jasperTemplateDto A template bound to the request body
   * @param templateId  UUID of template which we want to update
   * @return ResponseEntity containing the updated template
   */
  @RequestMapping(value = "/{id}", method = RequestMethod.PUT)
  public ResponseEntity<JasperTemplateDto> updateTemplate(
      @RequestBody JasperTemplateDto jasperTemplateDto, @PathVariable("id") UUID templateId) {
    JasperTemplate jasperTemplate = JasperTemplate.newInstance(jasperTemplateDto);
    JasperTemplate jasperTemplateToUpdate = jasperTemplateRepository.findOne(templateId);
    if (jasperTemplateToUpdate == null) {
      jasperTemplateToUpdate = new JasperTemplate();
      LOGGER.info("Creating new template");
    } else {
      LOGGER.debug("Updating template with id: " + templateId);
    }

    jasperTemplateToUpdate.updateFrom(jasperTemplate);
    jasperTemplateToUpdate = jasperTemplateRepository.save(jasperTemplateToUpdate);

    LOGGER.debug("Saved template with id: " + jasperTemplateToUpdate.getId());
    return new ResponseEntity<>(
        JasperTemplateDto.newInstance(jasperTemplateToUpdate), HttpStatus.OK);
  }

  /**
   * Get chosen template.
   *
   * @param templateId UUID of template which we want to get
   * @return Template.
   */
  @RequestMapping(value = "/{id}", method = RequestMethod.GET)
  public ResponseEntity<JasperTemplateDto> getTemplate(@PathVariable("id") UUID templateId) {
    JasperTemplate jasperTemplate =
        jasperTemplateRepository.findOne(templateId);
    if (jasperTemplate == null) {
      throw new ContentNotFoundMessageException(new Message(
          MessageKeys.ERROR_JASPER_TEMPLATE_NOT_FOUND, templateId));
    } else {
      return new ResponseEntity<>(JasperTemplateDto.newInstance(jasperTemplate), HttpStatus.OK);
    }
  }

  /**
   * Allows deleting template.
   *
   * @param templateId UUID of template which we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<JasperTemplateDto> deleteTemplate(@PathVariable("id") UUID templateId) {
    JasperTemplate jasperTemplate = jasperTemplateRepository.findOne(templateId);
    if (jasperTemplate == null) {
      throw new ContentNotFoundMessageException(new Message(
          MessageKeys.ERROR_JASPER_TEMPLATE_NOT_FOUND, templateId));
    } else {
      jasperTemplateRepository.delete(jasperTemplate);
      return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }
  }
}
