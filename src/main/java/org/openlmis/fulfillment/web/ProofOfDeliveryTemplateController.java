package org.openlmis.fulfillment.web;

import net.sf.jasperreports.engine.JRException;
import org.apache.commons.io.IOUtils;
import org.apache.log4j.Logger;
import org.openlmis.referencedata.web.BaseController;
import org.openlmis.reporting.exception.ReportingException;
import org.openlmis.reporting.model.Template;
import org.openlmis.reporting.service.TemplateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.multipart.MultipartFile;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.servlet.http.HttpServletResponse;

@Controller
public class ProofOfDeliveryTemplateController extends BaseController {

  private static final Logger LOGGER = Logger.getLogger(
      ProofOfDeliveryTemplateController.class);

  private static final String PRINT_POD = "Print POD";
  private static final String DESCRIPTION_POD = "Template to print Proof Of Delivery";
  private static final String CONSISTENCY_REPORT = "Consistency Report";

  @Autowired
  private TemplateService templateService;

  /**
   * Add Proof Of Delivery report templates with ".jrxml" format(extension) to database.
   *
   * @param file File in ".jrxml" format to add or upload.
   * @return ResponseEntity with the "#200 OK" HTTP response status on success
   *         or ResponseEntity containing the error description status.
   */
  @RequestMapping(value = "/proofOfDeliveryTemplates", method = RequestMethod.POST)
  public ResponseEntity<?> saveTemplateOfPod(@RequestPart("file") MultipartFile file) {
    Template template = new Template(PRINT_POD, null, null, CONSISTENCY_REPORT, DESCRIPTION_POD);
    try {
      templateService.validateFileAndSaveTemplate(template, file);
    } catch (ReportingException ex) {
      LOGGER.debug(ex);
      return new ResponseEntity<>(ex.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
    }
    return new ResponseEntity<>(HttpStatus.OK);
  }

  /**
   * Download report template with ".jrxml" format(extension) for Proof of Delivery from database.
   *
   * @param response HttpServletResponse object.
   */
  @RequestMapping(value = "/proofOfDeliveryTemplates", method = RequestMethod.GET)
  @ResponseBody
  public void downloadPodXmlTemlate(HttpServletResponse response) {
    Template podPrintTemplate = templateService.getByName(PRINT_POD);
    if (podPrintTemplate == null) {
      try {
        response.sendError(HttpServletResponse.SC_NOT_FOUND,
            "Proof Of Delivery template does not exist.");
      } catch (IOException messageEx) {
        LOGGER.info("Error sending error message to client.", messageEx);
      }
    } else {
      response.setContentType("application/xml");
      response.addHeader("Content-Disposition", "attachment; filename=podPrint" + ".jrxml");
      try {
        File file = templateService.convertJasperToXml(podPrintTemplate);
        InputStream inputStream = new BufferedInputStream(new FileInputStream(file));
        IOUtils.copy(inputStream, response.getOutputStream());
        response.flushBuffer();
      } catch (JRException ex) {
        LOGGER.debug("Error writing report to xml stream.", ex);
        try {
          response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, ex.getMessage());
        } catch (IOException messageEx) {
          LOGGER.info("Error sending error message to client.", messageEx);
        }
      } catch (IOException ex) {
        LOGGER.debug("Error writing jrxml file to output stream.", ex);
        try {
          response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, ex.getMessage());
        } catch (IOException messageEx) {
          LOGGER.info("Error sending error message to client.", messageEx);
        }
      }
    }
  }
}
