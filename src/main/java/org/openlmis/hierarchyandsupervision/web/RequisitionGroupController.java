package org.openlmis.hierarchyandsupervision.web;

import org.openlmis.hierarchyandsupervision.domain.RequisitionGroup;
import org.openlmis.hierarchyandsupervision.repository.RequisitionGroupRepository;
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
public class RequisitionGroupController {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionGroupController.class);

  @Autowired
  private RequisitionGroupRepository requisitionGroupRepository;

  /**
   * Allows creating new requisitionGroup.
   *
   * @param requisitionGroup A requisitionGroup bound to the request body
   * @return ResponseEntity containing the created requisitionGroup
   */
  @RequestMapping(value = "/requisitionGroups", method = RequestMethod.POST)
  public ResponseEntity<?> createRequisitionGroup(@RequestBody RequisitionGroup requisitionGroup) {
    if (requisitionGroup == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      LOGGER.debug("Creating new requisitionGroup");
      // Ignore provided id
      requisitionGroup.setId(null);
      RequisitionGroup newRequisitionGroup = requisitionGroupRepository.save(requisitionGroup);
      return new ResponseEntity<RequisitionGroup>(newRequisitionGroup, HttpStatus.CREATED);
    }
  }

  /**
   * Get all requisitionGroups.
   *
   * @return RequisitionGroups.
   */
  @RequestMapping(value = "/requisitionGroups", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllRequisitionGroup() {
    Iterable<RequisitionGroup> requisitionGroups = requisitionGroupRepository.findAll();
    if (requisitionGroups == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(requisitionGroups, HttpStatus.OK);
    }
  }

  /**
   * Get choosen requisitionGroup.
   *
   * @param requisitionGroupId UUID of requisitionGroup whose we want to get
   * @return RequisitionGroup.
   */
  @RequestMapping(value = "/requisitionGroups/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getRequisitionGroup(@PathVariable("id") UUID requisitionGroupId) {
    RequisitionGroup requisitionGroup = requisitionGroupRepository.findOne(requisitionGroupId);
    if (requisitionGroup == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(requisitionGroup, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting requisitionGroup.
   *
   * @param requisitionGroupId UUID of requisitionGroup whose we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/requisitionGroups/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteRequisitionGroup(@PathVariable("id") UUID requisitionGroupId) {
    RequisitionGroup requisitionGroup = requisitionGroupRepository.findOne(requisitionGroupId);
    if (requisitionGroupId == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        requisitionGroupRepository.delete(requisitionGroup);
      } catch (DataIntegrityViolationException ex) {
        LOGGER.debug("RequisitionGroup cannot be deleted because of existing dependencies", ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<RequisitionGroup>(HttpStatus.NO_CONTENT);
    }
  }
}
