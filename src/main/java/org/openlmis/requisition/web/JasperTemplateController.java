package org.openlmis.requisition.web;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.servlet.http.HttpServletRequest;
import org.apache.log4j.Logger;
import org.openlmis.requisition.domain.JasperTemplate;
import org.openlmis.requisition.domain.JasperTemplateParameter;
import org.openlmis.requisition.dto.JasperTemplateDto;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.JasperReportViewException;
import org.openlmis.requisition.exception.ReportingException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.repository.JasperTemplateRepository;
import org.openlmis.requisition.service.JasperReportsViewService;
import org.openlmis.requisition.service.JasperTemplateService;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.view.jasperreports.JasperReportsMultiFormatView;

@Controller
@RequestMapping("/api/reports/templates/requisitions")
public class JasperTemplateController extends BaseController {

  private static final String CONSISTENCY_REPORT = "Consistency Report";

  private static final Logger LOGGER = Logger.getLogger(JasperTemplateController.class);

  @Autowired
  private JasperTemplateService jasperTemplateService;

  @Autowired
  private JasperTemplateRepository jasperTemplateRepository;

  @Autowired
  private JasperReportsViewService jasperReportsViewService;

  @Autowired
  private PermissionService permissionService;

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

    permissionService.canEditReportTemplates();

    JasperTemplate jasperTemplateToUpdate = jasperTemplateRepository.findByName(name);
    if (jasperTemplateToUpdate == null) {
      LOGGER.debug("Creating new template");
      jasperTemplateToUpdate = new JasperTemplate(name, null, null, CONSISTENCY_REPORT,
          description);
      jasperTemplateService.validateFileAndInsertTemplate(jasperTemplateToUpdate, file);
    } else {
      LOGGER.debug("Template found, updating template");
      jasperTemplateToUpdate.setDescription(description);
      jasperTemplateService.validateFileAndSaveTemplate(jasperTemplateToUpdate, file);
    }

    LOGGER.debug("Saved template with id: " + jasperTemplateToUpdate.getId());
  }

  /**
   * Get all templates.
   *
   * @return Templates.
   */
  @RequestMapping(method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<Iterable<JasperTemplateDto>> getAllTemplates() {
    permissionService.canViewReports();
    Iterable<JasperTemplateDto> templates =
        JasperTemplateDto.newInstance(jasperTemplateRepository.findAll());
    return new ResponseEntity<>(templates, HttpStatus.OK);
  }

  /**
   * Get chosen template.
   *
   * @param templateId UUID of template which we want to get
   * @return Template.
   */
  @RequestMapping(value = "/{id}", method = RequestMethod.GET)
  public ResponseEntity<JasperTemplateDto> getTemplate(@PathVariable("id") UUID templateId) {
    permissionService.canViewReports();
    JasperTemplate jasperTemplate =
        jasperTemplateRepository.findOne(templateId);
    if (jasperTemplate == null) {
      throw new ContentNotFoundMessageException(new Message(
          MessageKeys.ERROR_JASPER_TEMPLATE_NOT_FOUND, templateId));
    } else {
      // run select sql and populate returned values for every template parameter
      for (JasperTemplateParameter tp : jasperTemplate.getTemplateParameters()) {
        if (isNotBlank(tp.getSelectSql())) {
          LOGGER.debug("Template Parameter " + jasperTemplate.getName() + " has select sql: "
              + tp.getSelectSql());
          List<String> selectValues = jasperTemplateRepository.runArbitrarySql(tp.getSelectSql());
          LOGGER.debug("Template Parameter " + jasperTemplate.getName() + " select values: "
              + selectValues);
          tp.setSelectValues(selectValues);
        }
      }
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
    permissionService.canEditReportTemplates();
    JasperTemplate jasperTemplate = jasperTemplateRepository.findOne(templateId);
    if (jasperTemplate == null) {
      throw new ContentNotFoundMessageException(new Message(
          MessageKeys.ERROR_JASPER_TEMPLATE_NOT_FOUND, templateId));
    } else {
      jasperTemplateRepository.delete(jasperTemplate);
      return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }
  }

  /**
   * Generate a report based on the template, the format and the request parameters.
   *
   * @param request    request (to get the request parameters)
   * @param templateId report template ID
   * @param format     report format to generate, default is PDF
   * @return the generated report
   */
  @RequestMapping(value = "/{id}/{format}", method = RequestMethod.GET)
  public ModelAndView generateReport(HttpServletRequest request,
      @PathVariable("id") UUID templateId,
      @PathVariable("format") String format) throws JasperReportViewException {

    permissionService.canViewReports();

    JasperTemplate template = jasperTemplateRepository.findOne(templateId);
    if (template == null) {
      throw new ContentNotFoundMessageException(new Message(
          MessageKeys.ERROR_JASPER_TEMPLATE_NOT_FOUND, templateId));
    }

    JasperReportsMultiFormatView jasperView = jasperReportsViewService
        .getJasperReportsViewWithJdbcDatasource(template, request);
    Map<String, Object> map = jasperTemplateService.mapRequestParametersToTemplate(
        request, template);
    map.put("format", format);

    return new ModelAndView(jasperView, map);
  }
}
