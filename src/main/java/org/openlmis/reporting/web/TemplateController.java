package org.openlmis.reporting.web;

import org.apache.log4j.Logger;
import org.openlmis.reporting.exception.ReportingException;
import org.openlmis.reporting.model.Template;
import org.openlmis.reporting.service.TemplateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;

@RepositoryRestController
public class TemplateController {

  private static final String CONSISTENCY_REPORT = "Consistency Report";

  private static final Logger LOGGER = Logger.getLogger(TemplateController.class);

  @Autowired
  private TemplateService templateService;

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
}
