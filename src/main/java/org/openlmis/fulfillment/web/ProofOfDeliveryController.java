package org.openlmis.fulfillment.web;

import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


@RepositoryRestController
public class ProofOfDeliveryController {

  @Autowired
  private ProofOfDeliveryRepository proofOfDeliveryRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  /**
   * Print to PDF Proof of Delivery.
   *
   * @param proofOfDeliveryId The UUID of the ProofOfDelivery to print
   * @return ResponseEntity with the "#200 OK" HTTP response status on success
   or ResponseEntity containing the error description and "#400 Bad Request" status.
   */
  @RequestMapping(value = "/proofOfDeliveries/{id}/print", method = RequestMethod.GET)
  @ResponseBody
  public ModelAndView print(HttpServletRequest request, HttpServletResponse response,
                            @PathVariable("id") UUID proofOfDeliveryId) throws Exception {

    ProofOfDelivery proofOfDelivery =
        proofOfDeliveryRepository.findOne(proofOfDeliveryId);

    Requisition requisition = findRequisition(proofOfDelivery);

    Map<Requisition, ProofOfDelivery> proofOfDeliveries = new HashMap<>();
    proofOfDeliveries.put(requisition, proofOfDelivery);

    ModelAndView modelAndView = new ModelAndView("pdfView", "orderProofOfDeliveries",
        proofOfDeliveries);

    return modelAndView;
  }

  private Requisition findRequisition(ProofOfDelivery proofOfDelivery) {
    if (proofOfDelivery.getOrder().getRequisitionCode() != null) {
      String requisitionCode = proofOfDelivery.getOrder().getRequisitionCode();
      UUID requisitionId = UUID.fromString(requisitionCode);
      Requisition requisition = requisitionRepository.findOne(requisitionId);
      return requisition;
    }
    return null;
  }
}
