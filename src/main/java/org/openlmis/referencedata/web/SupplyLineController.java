package org.openlmis.referencedata.web;

import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.SupplyLine;
import org.openlmis.referencedata.repository.SupplyLineRepository;
import org.openlmis.referencedata.service.SupplyLineService;
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
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.List;
import java.util.UUID;

@RepositoryRestController
public class SupplyLineController {

  private static final Logger LOGGER = LoggerFactory.getLogger(SupplyLineController.class);

  @Autowired
  private SupplyLineService supplyLineService;

  @Autowired
  private SupplyLineRepository supplyLineRepository;

  /**
   * Allows creating new supplyLines.
   *
   * @param supplyLine A supplyLine bound to the request body
   * @return ResponseEntity containing the created supplyLine
   */
  @RequestMapping(value = "/supplyLines", method = RequestMethod.POST)
  public ResponseEntity<?> createSupplyLine(@RequestBody SupplyLine supplyLine) {
    if (supplyLine == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      LOGGER.debug("Creating new supplyLine");
      // Ignore provided id
      supplyLine.setId(null);
      SupplyLine newSupplyLine = supplyLineRepository.save(supplyLine);
      return new ResponseEntity<SupplyLine>(newSupplyLine, HttpStatus.CREATED);
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
    if (supplyLines == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(supplyLines, HttpStatus.OK);
    }
  }

  /**
   * Get choosen supplyLine.
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
        LOGGER.debug("SupplyLine cannot be deleted because of existing dependencies", ex);
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
