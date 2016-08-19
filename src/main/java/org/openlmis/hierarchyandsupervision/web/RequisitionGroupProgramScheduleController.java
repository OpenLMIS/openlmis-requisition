package org.openlmis.hierarchyandsupervision.web;

import org.openlmis.hierarchyandsupervision.domain.RequisitionGroupProgramSchedule;
import org.openlmis.hierarchyandsupervision.repository.RequisitionGroupProgramScheduleRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.UUID;

@RepositoryRestController
public class RequisitionGroupProgramScheduleController {

  private static final Logger LOGGER =
        LoggerFactory.getLogger(RequisitionGroupProgramScheduleController.class);

  @Autowired
  private RequisitionGroupProgramScheduleRepository repository;

  /**
   * Allows creating new requisitionGroupProgramSchedule.
   *
   * @param requisition A requisitionGroupProgramSchedule bound to the request body
   * @return ResponseEntity containing the created requisitionGroupProgramSchedule
   */
  @RequestMapping(value = "/requisitionGroupProgramSchedules", method = RequestMethod.POST)
  public ResponseEntity<?> createRequisitionGroupProgramSchedule(
        @RequestBody RequisitionGroupProgramSchedule requisition) {
    if (requisition == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      LOGGER.debug("Creating new requisitionGPS");
      // Ignore provided id
      requisition.setId(null);
      RequisitionGroupProgramSchedule newRequisition = repository.save(requisition);
      return new ResponseEntity<RequisitionGroupProgramSchedule>(
            newRequisition, HttpStatus.CREATED);
    }
  }

  /**
   * Get all requisitionGroupProgramSchedules.
   *
   * @return RequisitionGroupProgramSchedules.
   */
  @RequestMapping(value = "/requisitionGroupProgramSchedules", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllRequisitionGroupProgramSchedule() {
    Iterable<RequisitionGroupProgramSchedule> requisitions = repository.findAll();
    if (requisitions == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(requisitions, HttpStatus.OK);
    }
  }

  /**
   * Get choosen requisitionGroupProgramSchedule.
   *
   * @param requisitionId UUID of requisitionGroupProgramSchedule whose we want to get
   * @return RequisitionGroupProgramSchedule.
   */
  @RequestMapping(value = "/requisitionGroupProgramSchedules/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getRequisitionGroupProgramSchedule(
        @PathVariable("id") UUID requisitionId) {
    RequisitionGroupProgramSchedule requisition = repository.findOne(requisitionId);
    if (requisition == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(requisition, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting requisitionGroupProgramSchedule.
   *
   * @param requisitionId UUID of requisitionGroupProgramSchedule whose we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/requisitionGroupProgramSchedules/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteRequisitionGroupProgramSchedule(
        @PathVariable("id") UUID requisitionId) {
    RequisitionGroupProgramSchedule requisition = repository.findOne(requisitionId);
    if (requisition == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        repository.delete(requisition);
      } catch (DataIntegrityViolationException ex) {
        LOGGER.debug("RequisitionGroupProgramSchedule cannot "
              + "be deleted because of existing dependencies", ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<RequisitionGroupProgramSchedule>(HttpStatus.NO_CONTENT);
    }
  }
}
