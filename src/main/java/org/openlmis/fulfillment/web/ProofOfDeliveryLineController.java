package org.openlmis.fulfillment.web;

import org.openlmis.fulfillment.domain.ProofOfDeliveryLine;
import org.openlmis.fulfillment.repository.ProofOfDeliveryLineRepository;
import org.openlmis.utils.ErrorResponse;
import org.openlmis.requisition.web.BaseController;
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
public class ProofOfDeliveryLineController extends BaseController {

  private static final Logger LOGGER =
        LoggerFactory.getLogger(ProofOfDeliveryLineController.class);

  @Autowired
  private ProofOfDeliveryLineRepository proofOfDeliveryLineRepository;


  /**
   * Allows creating new proofOfDeliveryLines.
   * If the id is specified, it will be ignored.
   *
   * @param proofOfDeliveryLine A proofOfDeliveryLine bound to the request body
   * @return ResponseEntity containing the created proofOfDeliveryLine
   */
  @RequestMapping(value = "/proofOfDeliveryLines", method = RequestMethod.POST)
  public ResponseEntity<?> createProofOfDeliveryLine(
          @RequestBody ProofOfDeliveryLine proofOfDeliveryLine) {
    try {
      LOGGER.debug("Creating new proofOfDeliveryLine");
      // Ignore provided id
      proofOfDeliveryLine.setId(null);
      ProofOfDeliveryLine newProofOfDeliveryLine
              = proofOfDeliveryLineRepository.save(proofOfDeliveryLine);
      LOGGER.debug("Created new proofOfDeliveryLine with id: " + proofOfDeliveryLine.getId());
      return new ResponseEntity<ProofOfDeliveryLine>(newProofOfDeliveryLine, HttpStatus.CREATED);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while creating proofOfDeliveryLine",
                  ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get all proofOfDeliveryLines.
   *
   * @return ProofOfDeliveryLine.
   */
  @RequestMapping(value = "/proofOfDeliveryLines", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllProofOfDeliveryLines() {
    Iterable<ProofOfDeliveryLine> proofOfDeliveryLines = proofOfDeliveryLineRepository.findAll();
    return new ResponseEntity<>(proofOfDeliveryLines, HttpStatus.OK);
  }

  /**
   * Allows updating proofOfDeliveryLines.
   *
   * @param proofOfDeliveryLine A proofOfDeliveryLine bound to the request body
   * @param proofOfDeliveryLineId UUID of proofOfDeliveryLine which we want to update
   * @return ResponseEntity containing the updated proofOfDeliveryLine
   */
  @RequestMapping(value = "/proofOfDeliveryLines/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateProofOfDeliveryLine(
        @RequestBody ProofOfDeliveryLine proofOfDeliveryLine,
        @PathVariable("id") UUID proofOfDeliveryLineId) {

    ProofOfDeliveryLine proofOfDeliveryLineToUpdate
          = proofOfDeliveryLineRepository.findOne(proofOfDeliveryLineId);
    try {
      if (proofOfDeliveryLineToUpdate == null) {
        proofOfDeliveryLineToUpdate = new ProofOfDeliveryLine();
        LOGGER.info("Creating new proofOfDeliveryLine");
      } else {
        LOGGER.debug("Updating proofOfDeliveryLine with id: " + proofOfDeliveryLineId);
      }

      proofOfDeliveryLineToUpdate.updateFrom(proofOfDeliveryLine);
      proofOfDeliveryLineToUpdate
            = proofOfDeliveryLineRepository.save(proofOfDeliveryLineToUpdate);

      LOGGER.debug("Saved proofOfDeliveryLine with id: " + proofOfDeliveryLineToUpdate.getId());
      return new ResponseEntity<ProofOfDeliveryLine>(proofOfDeliveryLineToUpdate, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while saving proofOfDeliveryLine with id: "
                  + proofOfDeliveryLineToUpdate.getId(), ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get chosen proofOfDeliveryLine.
   *
   * @param proofOfDeliveryLineId UUID of proofOfDeliveryLine whose we want to get
   * @return ProofOfDeliveryLine.
   */
  @RequestMapping(value = "/proofOfDeliveryLines/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getProofOfDeliveryLine(@PathVariable("id") UUID proofOfDeliveryLineId) {
    ProofOfDeliveryLine proofOfDeliveryLine
            = proofOfDeliveryLineRepository.findOne(proofOfDeliveryLineId);
    if (proofOfDeliveryLine == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(proofOfDeliveryLine, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting proofOfDeliveryLine.
   *
   * @param proofOfDeliveryLineId UUID of proofOfDeliveryLine whose we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/proofOfDeliveryLines/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteProofOfDeliveryLine(
          @PathVariable("id") UUID proofOfDeliveryLineId) {
    ProofOfDeliveryLine proofOfDeliveryLine
            = proofOfDeliveryLineRepository.findOne(proofOfDeliveryLineId);
    if (proofOfDeliveryLine == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        proofOfDeliveryLineRepository.delete(proofOfDeliveryLine);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("An error accurred while deleting proofOfDeliveryLine with id: "
                    + proofOfDeliveryLineId, ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<ProofOfDeliveryLine>(HttpStatus.NO_CONTENT);
    }
  }
}
