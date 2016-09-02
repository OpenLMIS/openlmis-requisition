package org.openlmis.requisition.web;

import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.web.BaseController;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.service.RequisitionLineService;
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
public class RequisitionLineController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionLineController.class);

  @Autowired
  private RequisitionLineService requisitionLineService;

  @Autowired
  private RequisitionLineRepository requisitionLineRepository;

  /**
   * Allows creating new requisitionLines.
   * If the id is specified, it will be ignored.
   *
   * @param requisitionLine A requisitionLine bound to the request body
   * @return ResponseEntity containing the created requisitionLine
   */
  @RequestMapping(value = "/requisitionLines", method = RequestMethod.POST)
  public ResponseEntity<?> createRequisitionLine(@RequestBody RequisitionLine requisitionLine) {
    try {
      LOGGER.debug("Creating new requisitionLine");
      requisitionLine.setId(null);
      RequisitionLine newRequisitionLine = requisitionLineRepository.save(requisitionLine);
      LOGGER.debug("Created new requisitionLine with id: " + requisitionLine.getId());
      return new ResponseEntity<RequisitionLine>(newRequisitionLine, HttpStatus.CREATED);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while creating requisitionLine", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get all requisitionLines.
   *
   * @return RequisitionLines.
   */
  @RequestMapping(value = "/requisitionLines", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllRequisitionLines() {
    Iterable<RequisitionLine> requisitionLines = requisitionLineRepository.findAll();
    return new ResponseEntity<>(requisitionLines, HttpStatus.OK);
  }

  /**
   * Allows updating requisitionLines.
   *
   * @param requisitionLine A requisitionLine bound to the request body
   * @param requisitionLineId UUID of requisitionLine which we want to update
   * @return ResponseEntity containing the updated requisitionLine
   */
  @RequestMapping(value = "/requisitionLines/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateRequisitionLines(@RequestBody RequisitionLine requisitionLine,
                                       @PathVariable("id") UUID requisitionLineId) {
    try {
      LOGGER.debug("Updating requisitionLine with id: " + requisitionLineId);

      RequisitionLine requisitionLineToUpdate =
            requisitionLineRepository.findOne(requisitionLineId);

      if (requisitionLineToUpdate == null) {
        requisitionLineToUpdate = new RequisitionLine();
        requisitionLineToUpdate.updateFrom(requisitionLine);
        requisitionLineToUpdate = requisitionLineRepository.save(requisitionLineToUpdate);
      } else {
        if (requisitionLineToUpdate.getRequisition().getStatus() == RequisitionStatus.INITIATED
              || requisitionLineToUpdate.getRequisition().getStatus()
              == RequisitionStatus.SUBMITTED) {
          requisitionLineToUpdate.updateFrom(requisitionLine);
          requisitionLineToUpdate = requisitionLineRepository.save(requisitionLineToUpdate);
        } else if (requisitionLineToUpdate.getRequisition().getStatus()
              == RequisitionStatus.AUTHORIZED) {
          requisitionLineToUpdate.setApprovedQuantity(requisitionLine.getApprovedQuantity());
          requisitionLineToUpdate.setRemarks(requisitionLine.getRemarks());
          requisitionLineToUpdate = requisitionLineRepository.save(requisitionLineToUpdate);
        }
      }
      LOGGER.debug("Updated requisitionLine with id: " + requisitionLineId);
      return new ResponseEntity<RequisitionLine>(requisitionLineToUpdate, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while updating requisitionLine with id: "
                  + requisitionLineId, ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get chosen requisitionLine.
   *
   * @param requisitionLineId UUID of requisitionLine which we want to get
   * @return RequisitionLine.
   */
  @RequestMapping(value = "/requisitionLines/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getRequisitionLine(@PathVariable("id") UUID requisitionLineId) {
    RequisitionLine requisitionLine = requisitionLineRepository.findOne(requisitionLineId);
    if (requisitionLine == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(requisitionLine, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting requisitionLine.
   *
   * @param requisitionLineId UUID of requisitionLine which we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/requisitionLines/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteRequisitionLine(@PathVariable("id") UUID requisitionLineId) {
    RequisitionLine requisitionLine = requisitionLineRepository.findOne(requisitionLineId);
    if (requisitionLine == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        requisitionLineRepository.delete(requisitionLine);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("An error accurred while deleting requisitionLine with id: "
                    + requisitionLineId, ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<RequisitionLine>(HttpStatus.NO_CONTENT);
    }
  }

  /**
   * Returns all requisition lines with matched parameters.
   * @param requisition requisition of searched requisition lines.
   * @param product product of searched requisition lines.
   * @return ResponseEntity with list of all requisition lines matching
   *         provided parameters and OK httpStatus.
   */
  @RequestMapping(value = "/requisitionLines/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchRequisitionLines(
      @RequestParam(value = "requisition", required = true) Requisition requisition,
      @RequestParam(value = "product", required = true) Product product) {
    List<RequisitionLine> result = requisitionLineService
        .searchRequisitionLines(requisition, product);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }
}
