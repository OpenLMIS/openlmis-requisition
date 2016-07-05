package org.openlmis.referencedata.web;

import org.openlmis.referencedata.domain.Requisition;
import org.openlmis.referencedata.domain.RequisitionStatus;
import org.openlmis.referencedata.repository.RequisitionRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@RepositoryRestController
public class RequisitionController {
  Logger logger = LoggerFactory.getLogger(RequisitionController.class);

  @Autowired
  private RequisitionRepository repository;

  /**
   * Submits earlier initiated requisition.
   */
  @RequestMapping(value = "/requisitions/submit", method = RequestMethod.POST)
  public ResponseEntity<?> submitRequisition(@RequestBody Requisition requisition) {
    if (requisition == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      logger.debug("Submitting a requisition");
      requisition.setStatus(RequisitionStatus.SUBMITTED);
      Requisition newRequisition = repository.save(requisition);
      return new ResponseEntity<Requisition>(newRequisition, HttpStatus.CREATED);
    }
  }
}