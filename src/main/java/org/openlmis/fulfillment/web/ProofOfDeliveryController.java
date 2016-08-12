package org.openlmis.fulfillment.web;

import net.sf.jasperreports.engine.JRException;
import org.openlmis.fulfillment.utils.ReportUtils;
import org.openlmis.reporting.model.Template;
import org.openlmis.reporting.service.JasperReportsViewFactory;
import org.openlmis.reporting.service.TemplateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.view.jasperreports.JasperReportsMultiFormatView;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.Map;
import java.util.UUID;

@RepositoryRestController
public class ProofOfDeliveryController {

  private static final String PRINT_POD = "Print POD";

  @Autowired
  private JasperReportsViewFactory jasperReportsViewFactory;

  @Autowired
  private TemplateService templateService;

  /**
   * Print to PDF Proof of Delivery.
   *
   * @param proofOfDeliveryId The UUID of the ProofOfDelivery to print
   * @return ResponseEntity with the "#200 OK" HTTP response status and Pdf file on success,
   *         or ResponseEntity containing the error description status.
   */
  @RequestMapping(value = "/proofOfDeliveries/{id}/print", method = RequestMethod.GET)
  @ResponseBody
  public ModelAndView print(HttpServletRequest request,
                            @PathVariable("id") UUID proofOfDeliveryId)
      throws JRException, IOException, ClassNotFoundException {

    Template podPrintTemplate = templateService.getByName(PRINT_POD);

    Map<String, Object> params = ReportUtils.createParametersMap();
    String formatId = "'" + proofOfDeliveryId + "'";
    params.put("pod_id", formatId);

    JasperReportsMultiFormatView jasperView =
        jasperReportsViewFactory.getJasperReportsView(podPrintTemplate, request);

    return new ModelAndView(jasperView, params);
  }
}
