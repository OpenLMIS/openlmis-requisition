package org.openlmis.referencedata.web;

import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
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
public class FacilityTypeController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(FacilityTypeController.class);

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  /**
   * Allows creating new facilityType.
   *
   * @param facilityType A facilityType bound to the request body
   * @return ResponseEntity containing the created facilityType
   */
  @RequestMapping(value = "/facilityTypes", method = RequestMethod.POST)
  public ResponseEntity<?> createFacilityType(@RequestBody FacilityType facilityType) {
    try {
      LOGGER.debug("Creating new facilityType");
      // Ignore provided id
      facilityType.setId(null);
      FacilityType newFacilityType = facilityTypeRepository.save(facilityType);
      return new ResponseEntity<FacilityType>(newFacilityType, HttpStatus.CREATED);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while creating facilityType", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get all facilityTypes.
   *
   * @return FacilityTypes.
   */
  @RequestMapping(value = "/facilityTypes", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllFacilityTypes() {
    Iterable<FacilityType> facilityTypes = facilityTypeRepository.findAll();
    if (facilityTypes == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(facilityTypes, HttpStatus.OK);
    }
  }

  /**
   * Allows updating facilityTypes.
   *
   * @param facilityType A facilityType bound to the request body
   * @param facilityTypeId UUID of facilityType which we want to update
   * @return ResponseEntity containing the updated facilityType
   */
  @RequestMapping(value = "/facilityTypes/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateFacilityType(@RequestBody FacilityType facilityType,
                                            @PathVariable("id") UUID facilityTypeId) {
    try {
      LOGGER.debug("Updating facility");
      FacilityType updatedFacilityType = facilityTypeRepository.save(facilityType);
      return new ResponseEntity<FacilityType>(updatedFacilityType, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while updating facilityType", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get chosen facilityType.
   *
   * @param facilityTypeId UUID of facilityType which we want to get
   * @return FacilityType.
   */
  @RequestMapping(value = "/facilityTypes/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getFacilityType(@PathVariable("id") UUID facilityTypeId) {
    FacilityType facilityType = facilityTypeRepository.findOne(facilityTypeId);
    if (facilityType == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(facilityType, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting facilityType.
   *
   * @param facilityTypeId UUID of facilityType which we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/facilityTypes/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteFacilityType(@PathVariable("id") UUID facilityTypeId) {
    FacilityType facilityType = facilityTypeRepository.findOne(facilityTypeId);
    if (facilityType == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        facilityTypeRepository.delete(facilityType);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("FacilityType cannot be deleted because of existing dependencies",
                    ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<FacilityType>(HttpStatus.NO_CONTENT);
    }
  }
}
