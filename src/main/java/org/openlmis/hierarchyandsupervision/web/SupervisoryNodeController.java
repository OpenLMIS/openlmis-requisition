package org.openlmis.hierarchyandsupervision.web;

import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.repository.SupervisoryNodeRepository;
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
public class SupervisoryNodeController {

  private static final Logger LOGGER = LoggerFactory.getLogger(SupervisoryNodeController.class);

  @Autowired
  private SupervisoryNodeRepository supervisoryNodeRepository;

  /**
   * Allows creating new supervisoryNode.
   *
   * @param supervisoryNode A supervisoryNode bound to the request body
   * @return ResponseEntity containing the created supervisoryNode
   */
  @RequestMapping(value = "/supervisoryNodes", method = RequestMethod.POST)
  public ResponseEntity<?> createSupervisoryNode(@RequestBody SupervisoryNode supervisoryNode) {
    if (supervisoryNode == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      LOGGER.debug("Creating new supervisoryNode");
      // Ignore provided id
      supervisoryNode.setId(null);
      SupervisoryNode newSupervisoryNode = supervisoryNodeRepository.save(supervisoryNode);
      return new ResponseEntity<SupervisoryNode>(newSupervisoryNode, HttpStatus.CREATED);
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
    if (supervisoryNodes == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(supervisoryNodes, HttpStatus.OK);
    }
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
    SupervisoryNode supervisoryNodeFromDb = supervisoryNodeRepository.findOne(supervisoryNodeId);
    if (supervisoryNodeFromDb == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      LOGGER.debug("Updating supervisoryNode");
      SupervisoryNode updatedSupervisoryNode = supervisoryNodeRepository.save(supervisoryNode);
      return new ResponseEntity<SupervisoryNode>(updatedSupervisoryNode, HttpStatus.OK);
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
        LOGGER.debug("SupervisoryNode cannot be deleted because of existing dependencies", ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<SupervisoryNode>(HttpStatus.NO_CONTENT);
    }
  }
}
