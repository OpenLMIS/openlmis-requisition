package org.openlmis.hierarchyandsupervision.web;

import org.openlmis.hierarchyandsupervision.domain.Right;
import org.openlmis.hierarchyandsupervision.repository.RightRepository;
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
import org.springframework.web.client.RestClientException;

import java.util.UUID;

@Controller
public class RightController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(RightController.class);

  @Autowired
  private RightRepository rightRepository;

  /**
   * Allows creating new right.
   *
   * @param right A right bound to the request body
   * @return ResponseEntity containing the created right
   */
  @RequestMapping(value = "/rights", method = RequestMethod.POST)
  public ResponseEntity<?> createRight(@RequestBody Right right) {
    try {
      LOGGER.debug("Creating new right");
      // Ignore provided id
      right.setId(null);
      Right newRight = rightRepository.save(right);
      return new ResponseEntity<Right>(newRight, HttpStatus.CREATED);
    } catch (RestClientException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while creating right", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get all rights.
   *
   * @return Rights.
   */
  @RequestMapping(value = "/rights", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllRights() {
    Iterable<Right> rights = rightRepository.findAll();
    if (rights == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(rights, HttpStatus.OK);
    }
  }

  /**
   * Allows updating rights.
   *
   * @param right A role bound to the request body
   * @param rightId UUID of role which we want to update
   * @return ResponseEntity containing the updated role
   */
  @RequestMapping(value = "/rights/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateRight(@RequestBody Right right,
                                      @PathVariable("id") UUID rightId) {
    try {
      LOGGER.debug("Updating right");
      Right updatedRight = rightRepository.save(right);
      return new ResponseEntity<Right>(updatedRight, HttpStatus.OK);
    } catch (RestClientException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while updating right", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get chosen right.
   *
   * @param rightId UUID of right whose we want to get
   * @return Right.
   */
  @RequestMapping(value = "/rights/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getRight(@PathVariable("id") UUID rightId) {
    Right right = rightRepository.findOne(rightId);
    if (right == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(right, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting right.
   *
   * @param rightId UUID of right whose we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/rights/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteRight(@PathVariable("id") UUID rightId) {
    Right right = rightRepository.findOne(rightId);
    if (right == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        rightRepository.delete(right);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("Right cannot be deleted because of existing dependencies",
                    ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<Right>(HttpStatus.NO_CONTENT);
    }
  }
}
