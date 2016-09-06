package org.openlmis.hierarchyandsupervision.web;

import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.domain.SupplyLine;
import org.openlmis.hierarchyandsupervision.repository.SupplyLineRepository;
import org.openlmis.hierarchyandsupervision.service.SupplyLineService;
import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
import org.openlmis.referencedata.domain.Program;
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
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.List;
import java.util.UUID;

@Controller
public class SupplyLineController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(SupplyLineController.class);

  @Autowired
  private SupplyLineService supplyLineService;

  @Autowired
  private SupplyLineRepository supplyLineRepository;

  /**
   * Allows creating new supplyLines.
   * If the id is specified, it will be ignored.
   *
   * @param supplyLine A supplyLine bound to the request body
   * @return ResponseEntity containing the created supplyLine
   */
  @RequestMapping(value = "/supplyLines", method = RequestMethod.POST)
  public ResponseEntity<?> createSupplyLine(@RequestBody SupplyLine supplyLine) {
    try {
      LOGGER.debug("Creating new supplyLine");
      supplyLine.setId(null);
      SupplyLine newSupplyLine = supplyLineRepository.save(supplyLine);
      LOGGER.debug("Created new supplyLine with id: " + supplyLine.getId());
      return new ResponseEntity<SupplyLine>(newSupplyLine, HttpStatus.CREATED);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while creating supplyLine", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get all supplyLines.
   *
   * @return SupplyLines.
   */
  @RequestMapping(value = "/supplyLines", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllSupplyLines() {
    Iterable<SupplyLine> supplyLines = supplyLineRepository.findAll();
    return new ResponseEntity<>(supplyLines, HttpStatus.OK);
  }

  /**
   * Allows updating supplyLines.
   *
   * @param supplyLine A supplyLine bound to the request body
   * @param supplyLineId UUID of supplyLine which we want to update
   * @return ResponseEntity containing the updated supplyLine
   */
  @RequestMapping(value = "/supplyLines/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateSupplyLine(@RequestBody SupplyLine supplyLine,
                                       @PathVariable("id") UUID supplyLineId) {

    SupplyLine supplyLineToUpdate = supplyLineRepository.findOne(supplyLineId);
    try {
      if (supplyLineToUpdate == null) {
        supplyLineToUpdate = new SupplyLine();
        LOGGER.info("Creating new supplyLine");
      } else {
        LOGGER.debug("Updating supplyLine with id: " + supplyLineId);
      }

      supplyLineToUpdate.updateFrom(supplyLine);
      supplyLineToUpdate = supplyLineRepository.save(supplyLineToUpdate);

      LOGGER.debug("Saved supplyLine with id: " + supplyLineToUpdate.getId());
      return new ResponseEntity<SupplyLine>(supplyLineToUpdate, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while saving supplyLine with id: "
                  + supplyLineToUpdate.getId(), ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get chosen supplyLine.
   *
   * @param supplyLineId UUID of supplyLine which we want to get
   * @return SupplyLine.
   */
  @RequestMapping(value = "/supplyLines/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getSupplyLine(@PathVariable("id") UUID supplyLineId) {
    SupplyLine supplyLine = supplyLineRepository.findOne(supplyLineId);
    if (supplyLine == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(supplyLine, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting supplyLine.
   *
   * @param supplyLineId UUID of supplyLine which we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/supplyLines/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteSupplyLine(@PathVariable("id") UUID supplyLineId) {
    SupplyLine supplyLine = supplyLineRepository.findOne(supplyLineId);
    if (supplyLine == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        supplyLineRepository.delete(supplyLine);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("An error accurred while deleting supplyLine with id: "
                    + supplyLineId, ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<SupplyLine>(HttpStatus.NO_CONTENT);
    }
  }

  /**
   * Returns all Supply Lines with matched parameters.
   * @param program program of searched Supply Lines.
   * @param supervisoryNode supervisory node of searched Supply Lines.
   * @return ResponseEntity with list of all Supply Lines matching
   *         provided parameters and OK httpStatus.
   */
  @RequestMapping(value = "/supplyLines/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchSupplyLines(
      @RequestParam(value = "program", required = true) Program program,
      @RequestParam(value = "supervisoryNode", required = true) SupervisoryNode supervisoryNode) {
    List<SupplyLine> result = supplyLineService.searchSupplyLines(program, supervisoryNode);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }
}
