package org.openlmis.hierarchyandsupervision.web;

import org.openlmis.hierarchyandsupervision.domain.RequisitionGroup;
import org.openlmis.hierarchyandsupervision.repository.RequisitionGroupRepository;
import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
import org.openlmis.referencedata.web.BaseController;
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

import java.util.UUID;

@Controller
public class RequisitionGroupController extends BaseController {

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
    try {
      LOGGER.debug("Creating new requisitionGroup");
      // Ignore provided id
      requisitionGroup.setId(null);
      RequisitionGroup newRequisitionGroup = requisitionGroupRepository.save(requisitionGroup);
      LOGGER.debug("Created new requisitionGroup with id: " + requisitionGroup.getId());
      return new ResponseEntity<RequisitionGroup>(newRequisitionGroup, HttpStatus.CREATED);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while creating requisitionGroup",
                  ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
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
    return new ResponseEntity<>(requisitionGroups, HttpStatus.OK);
  }

  /**
   * Get chosen requisitionGroup.
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
   * Allows updating requisitionGroup.
   *
   * @param requisitionGroup A requisitionGroup bound to the request body
   * @param requisitionGroupId UUID of requisitionGroup which we want to update
   * @return ResponseEntity containing the updated requisitionGroup
   */
  @RequestMapping(value = "/requisitionGroups/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateRequisitionGroup(@RequestBody RequisitionGroup requisitionGroup,
                                                 @PathVariable("id") UUID requisitionGroupId) {
    try {
      LOGGER.debug("Updating requisitionGroup with id: " + requisitionGroupId);
      RequisitionGroup updatedRequisitionGroup = requisitionGroupRepository.save(requisitionGroup);
      LOGGER.debug("Updated requisitionGroup with id: " + requisitionGroupId);
      return new ResponseEntity<RequisitionGroup>(updatedRequisitionGroup, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while updating requisitionGroup",
                  ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
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
        ErrorResponse errorResponse =
              new ErrorResponse("RequisitionGroup cannot be deleted"
                    + "because of existing dependencies", ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<RequisitionGroup>(HttpStatus.NO_CONTENT);
    }
  }
}
