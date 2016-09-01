package org.openlmis.hierarchyandsupervision.web;

import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.repository.SupervisoryNodeRepository;
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
public class SupervisoryNodeController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(SupervisoryNodeController.class);

  @Autowired
  private SupervisoryNodeRepository supervisoryNodeRepository;

  /**
   * Allows creating new supervisoryNode.
   * If the id is specified, it will be ignored.
   *
   * @param supervisoryNode A supervisoryNode bound to the request body
   * @return ResponseEntity containing the created supervisoryNode
   */
  @RequestMapping(value = "/supervisoryNodes", method = RequestMethod.POST)
  public ResponseEntity<?> createSupervisoryNode(@RequestBody SupervisoryNode supervisoryNode) {
    try {
      LOGGER.debug("Creating new supervisoryNode");
      supervisoryNode.setId(null);
      SupervisoryNode newSupervisoryNode = supervisoryNodeRepository.save(supervisoryNode);
      LOGGER.debug("Created new supervisoryNode with id: " + supervisoryNode.getId());
      return new ResponseEntity<SupervisoryNode>(newSupervisoryNode, HttpStatus.CREATED);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error occurred while saving supervisoryNode", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get all supervisoryNodes.
   *
   * @return SupervisoryNodes.
   */
  @RequestMapping(value = "/supervisoryNodes", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllSupervisoryNodes() {
    Iterable<SupervisoryNode> supervisoryNodes = supervisoryNodeRepository.findAll();
    return new ResponseEntity<>(supervisoryNodes, HttpStatus.OK);
  }

  /**
   * Get chosen supervisoryNode.
   *
   * @param supervisoryNodeId UUID of supervisoryNode whose we want to get
   * @return SupervisoryNode.
   */
  @RequestMapping(value = "/supervisoryNodes/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getSupervisoryNode(@PathVariable("id") UUID supervisoryNodeId) {
    SupervisoryNode supervisoryNode = supervisoryNodeRepository.findOne(supervisoryNodeId);
    if (supervisoryNode == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(supervisoryNode, HttpStatus.OK);
    }
  }

  /**
   * Allows updating supervisoryNode.
   *
   * @param supervisoryNode A supervisoryNode bound to the request body
   * @param supervisoryNodeId UUID of supervisoryNode which we want to update
   * @return ResponseEntity containing the updated supervisoryNode
   */
  @RequestMapping(value = "/supervisoryNodes/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateSupervisoryNode(@RequestBody SupervisoryNode supervisoryNode,
                                       @PathVariable("id") UUID supervisoryNodeId) {
    try {
      LOGGER.debug("Updating supervisoryNode with id: " + supervisoryNodeId);

      SupervisoryNode supervisoryNodeToUpdate =
            supervisoryNodeRepository.findOne(supervisoryNodeId);

      if (supervisoryNodeToUpdate == null) {
        supervisoryNodeToUpdate = new SupervisoryNode();
      }

      supervisoryNodeToUpdate.updateFrom(supervisoryNode);
      supervisoryNodeToUpdate = supervisoryNodeRepository.save(supervisoryNodeToUpdate);

      LOGGER.debug("Updated supervisoryNode with id: " + supervisoryNodeId);
      return new ResponseEntity<SupervisoryNode>(supervisoryNodeToUpdate, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error occurred while updating supervisoryNode", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Allows deleting supervisoryNode.
   *
   * @param supervisoryNodeId UUID of supervisoryNode whose we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/supervisoryNodes/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteSupervisoryNode(@PathVariable("id") UUID supervisoryNodeId) {
    SupervisoryNode supervisoryNode = supervisoryNodeRepository.findOne(supervisoryNodeId);
    if (supervisoryNode == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        supervisoryNodeRepository.delete(supervisoryNode);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("An error occurred while deleting supervisoryNode",
                    ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<SupervisoryNode>(HttpStatus.NO_CONTENT);
    }
  }
}
