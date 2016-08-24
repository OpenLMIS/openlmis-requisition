package org.openlmis.referencedata.web;

import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
import org.openlmis.referencedata.domain.FacilityOperator;
import org.openlmis.referencedata.repository.FacilityOperatorRepository;
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
import org.springframework.web.client.RestClientException;

import java.util.UUID;

@RepositoryRestController
public class FacilityOperatorController {

  private static final Logger LOGGER = LoggerFactory.getLogger(FacilityOperatorController.class);

  @Autowired
  FacilityOperatorRepository facilityOperatorRepository;


  /**
   * Allows creating new facilityOperators.
   *
   * @param facilityOperator A facilityOperator bound to the request body.
   * @return ResponseEntity containing the created facilityOperator.
   */
  @RequestMapping(value = "/facilityOperators", method = RequestMethod.POST)
  public ResponseEntity<?> createFacilityOperator(@RequestBody FacilityOperator facilityOperator) {
    try {
      LOGGER.debug("Creating new facility operator");
      // Ignore provided id
      facilityOperator.setId(null);
      FacilityOperator newFacilityOperator = facilityOperatorRepository.save(facilityOperator);
      return new ResponseEntity<FacilityOperator>(newFacilityOperator, HttpStatus.CREATED);
    } catch (RestClientException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while creating facilityOperator",
                  ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get all facilityOperators.
   *
   * @return facilityOperators.
   */
  @RequestMapping(value = "/facilityOperators", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllfacilityOperators() {
    Iterable<FacilityOperator> facilityOperators = facilityOperatorRepository.findAll();
    if (facilityOperators == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(facilityOperators, HttpStatus.OK);
    }
  }

  /**
   * Allows updating facilityOperator.
   *
   * @param facilityOperator A facilityOperator bound to the request body
   * @param facilityOperatorId UUID of facilityOperator which we want to update
   * @return ResponseEntity containing the updated facilityOperator
   */
  @RequestMapping(value = "/facilityOperators/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateFacilityOperators(@RequestBody FacilityOperator facilityOperator,
                                       @PathVariable("id") UUID facilityOperatorId) {
    try {
      LOGGER.debug("Updating facility operator");
      FacilityOperator updatedFacilityOperator = facilityOperatorRepository.save(facilityOperator);
      return new ResponseEntity<FacilityOperator>(updatedFacilityOperator, HttpStatus.OK);
    } catch (RestClientException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while updating facilityOperator",
                  ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get chosen facilityOperator.
   *
   * @param facilityOperatorId UUID of facilityOperator whose we want to get
   * @return facilityOperator.
   */
  @RequestMapping(value = "/facilityOperators/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getFacilityOperators(@PathVariable("id") UUID facilityOperatorId) {
    FacilityOperator facilityOperator = facilityOperatorRepository.findOne(facilityOperatorId);
    if (facilityOperator == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(facilityOperator, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting facilityOperator.
   *
   * @param facilityOperatorId UUID of facilityOperator whose we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/facilityOperators/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteFacilityOperators(@PathVariable("id") UUID facilityOperatorId) {
    FacilityOperator facilityOperator = facilityOperatorRepository.findOne(facilityOperatorId);
    if (facilityOperator == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        facilityOperatorRepository.delete(facilityOperator);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("FacilityOperator cannot be deleted"
                    + "because of existing dependencies", ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<FacilityOperator>(HttpStatus.NO_CONTENT);
    }
  }
}
