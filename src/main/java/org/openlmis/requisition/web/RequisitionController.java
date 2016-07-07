package org.openlmis.requisition.web;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.validate.RequisitionValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.util.HashMap;
import java.util.Map;

@RepositoryRestController
public class RequisitionController {
  Logger logger = LoggerFactory.getLogger(RequisitionController.class);

  @Autowired
  RequisitionRepository requisitionRepository;

  @Autowired
  @Qualifier("beforeSaveRequisitionValidator")
  RequisitionValidator validator;

  /**
   * Submits earlier initiated requisition.
   */
  @RequestMapping(value = "/requisitions/submit", method = RequestMethod.POST)
  public ResponseEntity<?> submitRequisition(@RequestBody Requisition requisition,
                                             BindingResult bindingResult) {
    if (requisition == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      validator.validate(requisition, bindingResult);

      if (bindingResult.getErrorCount() == 0) {
        logger.debug("Submitting a requisition");
        requisition.setStatus(RequisitionStatus.SUBMITTED);
        Requisition newRequisition = requisitionRepository.save(requisition);
        return new ResponseEntity<>(newRequisition, HttpStatus.CREATED);
      } else {
        return new ResponseEntity(getRequisitionErrors(bindingResult), HttpStatus.BAD_REQUEST);
      }
    }
  }

  private Map<String, String> getRequisitionErrors(BindingResult bindingResult) {
    return new HashMap<String, String>() {
      {
        for (FieldError error : bindingResult.getFieldErrors()) {
          put(error.getField(), error.getDefaultMessage());
        }
      }
    };
  }
}