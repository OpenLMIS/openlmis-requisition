package org.openlmis.requisition.web;

import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.validate.RequisitionValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

@RepositoryRestController
public class RequisitionController {
  Logger logger = LoggerFactory.getLogger(RequisitionController.class);

  @Autowired
  RequisitionRepository requisitionRepository;

  @Autowired
  @Qualifier("beforeSaveRequisitionValidator")
  RequisitionValidator validator;

  @Autowired
  RequisitionService requisitionService;

  @PersistenceContext
  EntityManager entityManager;

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

  /**
   * Finds requisitions matching all of provided parameters.
   */
  @RequestMapping(value = "/requisitions/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchRequisitions(
      @RequestParam(value = "facility", required = false) Facility facility,
      @RequestParam(value = "program", required = false) Program program,
      @RequestParam(value = "createdDateFrom", required = false)
      @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime createdDateFrom,
      @RequestParam(value = "createdDateTo", required = false)
      @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime createdDateTo) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Requisition> query = builder.createQuery(Requisition.class);
    Root<Requisition> root = query.from(Requisition.class);

    Predicate predicate = builder.conjunction();
    if (facility != null) {
      predicate = builder.and(predicate, builder.equal(root.get("facility"), facility));
    }
    if (program != null) {
      predicate = builder.and(predicate, builder.equal(root.get("program"), program));
    }
    if (createdDateFrom != null) {
      predicate = builder.and(predicate,
          builder.greaterThanOrEqualTo(root.get("createdDate"), createdDateFrom));
    }
    if (createdDateTo != null) {
      predicate = builder.and(predicate,
          builder.lessThanOrEqualTo(root.get("createdDate"), createdDateTo));
    }

    query.where(predicate);
    List<Requisition> result = entityManager.createQuery(query).getResultList();

    return new ResponseEntity<>(result, HttpStatus.OK);
  }

  /**
   * Skipping chosen requisition period.
     */
  @RequestMapping(value = "/requisitions/skip", method = RequestMethod.POST)
  public ResponseEntity<?> skipRequisition(@RequestBody Requisition requisition) {
    boolean skipped = requisitionService.skip(requisition);
    ResponseEntity<Object> responseEntity;
    if (skipped) {
      responseEntity = new ResponseEntity<Object>(requisition, HttpStatus.ACCEPTED);
    } else {
      responseEntity = new ResponseEntity<Object>(requisition, HttpStatus.BAD_REQUEST);
    }
    return responseEntity;
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