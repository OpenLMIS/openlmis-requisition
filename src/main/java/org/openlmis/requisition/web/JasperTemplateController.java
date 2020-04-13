/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org.
 */

package org.openlmis.requisition.web;

import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.UUID;
import javax.servlet.http.HttpServletRequest;
import org.openlmis.requisition.domain.JasperTemplate;
import org.openlmis.requisition.dto.JasperTemplateDto;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.JasperReportViewException;
import org.openlmis.requisition.exception.ReportingException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.repository.JasperTemplateRepository;
import org.openlmis.requisition.service.JasperReportsViewService;
import org.openlmis.requisition.service.JasperTemplateService;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.utils.Message;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.multipart.MultipartFile;

@Controller
@Transactional
@RequestMapping("/api/reports/templates/requisitions")
public class JasperTemplateController extends BaseController {

  private static final String CONSISTENCY_REPORT = "Consistency Report";
  private static final String TIMELINESS_REPORT = "Timeliness Report";
  private static final String REPORTING_RATE_REPORT = "Reporting Rate Report";
  private static final int DUE_DAYS = 10;

  private static final Logger LOGGER = LoggerFactory.getLogger(JasperTemplateController.class);

  @Autowired
  private JasperTemplateService jasperTemplateService;

  @Autowired
  private JasperTemplateRepository jasperTemplateRepository;

  @Autowired
  private JasperReportsViewService jasperReportsViewService;

  @Autowired
  private PermissionService permissionService;

  @Value("${dateTimeFormat}")
  private String dateTimeFormat;

  @Value("${time.zoneId}")
  private String timeZoneId;

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
    permissionService.canEditReportTemplates().throwExceptionIfHasErrors();

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
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public Iterable<JasperTemplateDto> getAllTemplates() {
    permissionService.canViewReports().throwExceptionIfHasErrors();
    Iterable<JasperTemplateDto> templates =
        JasperTemplateDto.newInstance(jasperTemplateRepository.findAll());
    return templates;
  }

  /**
   * Get chosen template.
   *
   * @param templateId UUID of template which we want to get
   * @return Template.
   */
  @RequestMapping(value = "/{id}", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  @ResponseBody
  public JasperTemplateDto getTemplate(@PathVariable("id") UUID templateId) {
    permissionService.canViewReports().throwExceptionIfHasErrors();
    JasperTemplate jasperTemplate = jasperTemplateRepository.findById(templateId)
        .orElseThrow(() -> new ContentNotFoundMessageException(new Message(
            MessageKeys.ERROR_JASPER_TEMPLATE_NOT_FOUND, templateId)));

    return JasperTemplateDto.newInstance(jasperTemplate);
  }

  /**
   * Allows deleting template.
   *
   * @param templateId UUID of template which we want to delete
   */
  @RequestMapping(value = "/{id}", method = RequestMethod.DELETE)
  @ResponseStatus(HttpStatus.NO_CONTENT)
  public void deleteTemplate(@PathVariable("id") UUID templateId) {
    permissionService.canEditReportTemplates().throwExceptionIfHasErrors();
    JasperTemplate jasperTemplate = jasperTemplateRepository.findById(templateId)
        .orElseThrow(() -> new ContentNotFoundMessageException(new Message(
            MessageKeys.ERROR_JASPER_TEMPLATE_NOT_FOUND, templateId)));

    jasperTemplateRepository.delete(jasperTemplate);
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
  @ResponseBody
  public ResponseEntity<byte[]> generateReport(HttpServletRequest request,
      @PathVariable("id") UUID templateId,
      @PathVariable("format") String format) throws JasperReportViewException {
    permissionService.canViewReports().throwExceptionIfHasErrors();

    JasperTemplate template = jasperTemplateRepository.findById(templateId)
        .orElseThrow(() -> new ContentNotFoundMessageException(new Message(
            MessageKeys.ERROR_JASPER_TEMPLATE_NOT_FOUND, templateId)));

    Map<String, Object> map = jasperTemplateService
        .mapRequestParametersToTemplate(request, template);
    map.put("format", format);
    map.put("dateTimeFormat", dateTimeFormat);
    map.put("timeZoneId", timeZoneId);

    byte[] reportData;
    
    if (TIMELINESS_REPORT.equals(template.getType())) {
      reportData = jasperReportsViewService.generateTimelinessReport(template, map);
    } else if (REPORTING_RATE_REPORT.equals(template.getType())) {
      map.putIfAbsent("DueDays", String.valueOf(DUE_DAYS));
      reportData = jasperReportsViewService.generateReportingRateReport(template, map);
    } else {
      reportData = jasperReportsViewService.generateReport(template, map);
    }

    MediaType mediaType;
    if ("csv".equals(format)) {
      mediaType = new MediaType("text", "csv", StandardCharsets.UTF_8);
    } else if ("xls".equals(format)) {
      mediaType = new MediaType("application", "vnd.ms-excel", StandardCharsets.UTF_8);
    } else if ("html".equals(format)) {
      mediaType = new MediaType("text", "html", StandardCharsets.UTF_8);
    } else {
      mediaType = new MediaType("application", "pdf", StandardCharsets.UTF_8);
    }
    String fileName = template.getName().replaceAll("\\s+", "_");

    return ResponseEntity
        .ok()
        .contentType(mediaType)
        .header("Content-Disposition", "inline; filename=" + fileName + "." + format)
        .body(reportData);
  }
}
