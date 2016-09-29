package org.openlmis.fulfillment.web;

import org.apache.commons.io.IOUtils;
import org.openlmis.reporting.exception.ReportingException;
import org.openlmis.reporting.model.Template;
import org.openlmis.reporting.service.TemplateService;
import org.openlmis.requisition.web.BaseController;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

@Controller
public class ProofOfDeliveryTemplateController extends BaseController {

  private static final String PRINT_POD = "Print POD";
  private static final String DESCRIPTION_POD = "Template to print Proof Of Delivery";
  private static final String CONSISTENCY_REPORT = "Consistency Report";

  @Autowired
  private TemplateService templateService;

  /**
   * Add Proof Of Delivery report templates with ".jrxml" format(extension) to database.
   *
   * @param file File in ".jrxml" format to add or upload.
   */
  @RequestMapping(value = "/proofOfDeliveryTemplates", method = RequestMethod.POST)
  @ResponseStatus(HttpStatus.OK)
  public void saveTemplateOfPod(@RequestPart("file") MultipartFile file)
          throws ReportingException {
    Template template = new Template(PRINT_POD, null, null, CONSISTENCY_REPORT, DESCRIPTION_POD);
    templateService.validateFileAndSaveTemplate(template, file);
  }

  /**
   * Download report template with ".jrxml" format(extension) for Proof of Delivery from database.
   *
   * @param response HttpServletResponse object.
   */
  @RequestMapping(value = "/proofOfDeliveryTemplates", method = RequestMethod.GET)
  @ResponseBody
  public void downloadPodXmlTemlate(HttpServletResponse response)
          throws IOException, ReportingException {
    Template podPrintTemplate = templateService.getByName(PRINT_POD);
    if (podPrintTemplate == null) {
      response.sendError(HttpServletResponse.SC_NOT_FOUND,
          "Proof Of Delivery template does not exist.");
    } else {
      response.setContentType("application/xml");
      response.addHeader("Content-Disposition", "attachment; filename=podPrint" + ".jrxml");

      File file = templateService.convertJasperToXml(podPrintTemplate);

      try (InputStream fis = new FileInputStream(file);
           InputStream bis = new BufferedInputStream(fis)) {

        IOUtils.copy(bis, response.getOutputStream());
        response.flushBuffer();
      } catch (IOException ex) {
        throw new ReportingException("Error writing jrxml file to output stream.", ex);
      }
    }
  }
}
