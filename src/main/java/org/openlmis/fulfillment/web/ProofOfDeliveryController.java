package org.openlmis.fulfillment.web;

import net.sf.jasperreports.engine.JRException;
import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
import org.openlmis.fulfillment.utils.ReportUtils;
import org.openlmis.utils.ErrorResponse;
import org.openlmis.requisition.web.BaseController;
import org.openlmis.reporting.model.Template;
import org.openlmis.reporting.service.JasperReportsViewFactory;
import org.openlmis.reporting.service.TemplateService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.view.jasperreports.JasperReportsMultiFormatView;

import java.io.IOException;
import java.util.Map;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;

@Controller
public class ProofOfDeliveryController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProofOfDeliveryController.class);
  private static final String PRINT_POD = "Print POD";

  @Autowired
  private JasperReportsViewFactory jasperReportsViewFactory;

  @Autowired
  private TemplateService templateService;

  @Autowired
  private ProofOfDeliveryRepository proofOfDeliveryRepository;


  /**
   * Allows creating new proofOfDeliveries.
   * If the id is specified, it will be ignored.
   *
   * @param proofOfDelivery A proofOfDelivery bound to the request body
   * @return ResponseEntity containing the created proofOfDelivery
   */
  @RequestMapping(value = "/proofOfDeliveries", method = RequestMethod.POST)
  public ResponseEntity<?> createProofOfDelivery(@RequestBody ProofOfDelivery proofOfDelivery) {
    try {
      LOGGER.debug("Creating new proofOfDelivery");
      proofOfDelivery.setId(null);
      ProofOfDelivery newProofOfDelivery = proofOfDeliveryRepository.save(proofOfDelivery);
      LOGGER.debug("Created new proofOfDelivery with id: " + proofOfDelivery.getId());
      return new ResponseEntity<ProofOfDelivery>(newProofOfDelivery, HttpStatus.CREATED);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error occurred while saving proofOfDelivery", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get all proofOfDeliveries.
   *
   * @return ProofOfDeliveries.
   */
  @RequestMapping(value = "/proofOfDeliveries", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllProofOfDeliveries() {
    Iterable<ProofOfDelivery> proofOfDeliveries = proofOfDeliveryRepository.findAll();
    return new ResponseEntity<>(proofOfDeliveries, HttpStatus.OK);
  }

  /**
   * Allows updating proofOfDeliveries.
   *
   * @param proofOfDelivery A proofOfDelivery bound to the request body
   * @param proofOfDeliveryId UUID of proofOfDelivery which we want to update
   * @return ResponseEntity containing the updated proofOfDelivery
   */
  @RequestMapping(value = "/proofOfDeliveries/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateProofOfDelivery(@RequestBody ProofOfDelivery proofOfDelivery,
                                       @PathVariable("id") UUID proofOfDeliveryId) {

    ProofOfDelivery proofOfDeliveryToUpdate =
          proofOfDeliveryRepository.findOne(proofOfDeliveryId);
    try {
      if (proofOfDeliveryToUpdate == null) {
        proofOfDeliveryToUpdate = new ProofOfDelivery();
        LOGGER.info("Creating new proofOfDelivery");
      } else {
        LOGGER.debug("Updating proofOfDelivery with id: " + proofOfDeliveryId);
      }

      proofOfDeliveryToUpdate.updateFrom(proofOfDelivery);
      proofOfDeliveryToUpdate = proofOfDeliveryRepository.save(proofOfDeliveryToUpdate);

      LOGGER.debug("Saved proofOfDelivery with id: " + proofOfDeliveryToUpdate.getId());
      return new ResponseEntity<ProofOfDelivery>(proofOfDeliveryToUpdate, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error occurred while saving proofOfDelivery with id: "
                  + proofOfDeliveryToUpdate.getId(), ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get chosen proofOfDelivery.
   *
   * @param proofOfDeliveryId UUID of proofOfDelivery whose we want to get
   * @return ProofOfDelivery.
   */
  @RequestMapping(value = "/proofOfDeliveries/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getProofOfDelivery(@PathVariable("id") UUID proofOfDeliveryId) {
    ProofOfDelivery proofOfDelivery = proofOfDeliveryRepository.findOne(proofOfDeliveryId);
    if (proofOfDelivery == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(proofOfDelivery, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting proofOfDelivery.
   *
   * @param proofOfDeliveryId UUID of proofOfDelivery whose we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/proofOfDeliveries/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteProofOfDelivery(@PathVariable("id") UUID proofOfDeliveryId) {
    ProofOfDelivery proofOfDelivery = proofOfDeliveryRepository.findOne(proofOfDeliveryId);
    if (proofOfDelivery == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        proofOfDeliveryRepository.delete(proofOfDelivery);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("An error occurred while deleting proofOfDelivery with id: "
                    + proofOfDeliveryId, ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<ProofOfDelivery>(HttpStatus.NO_CONTENT);
    }
  }

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
