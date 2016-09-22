package org.openlmis.requisition.web;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionLineItemRepository;
import org.openlmis.requisition.service.RequisitionLineItemService;
import org.openlmis.utils.ErrorResponse;
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
public class RequisitionLineItemController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionLineItemController.class);

  @Autowired
  private RequisitionLineItemService requisitionLineItemService;

  @Autowired
  private RequisitionLineItemRepository requisitionLineItemRepository;

  /**
   * Allows creating new requisitionLineItems.
   * If the id is specified, it will be ignored.
   *
   * @param requisitionLineItem A requisitionLineItem bound to the request body
   * @return ResponseEntity containing the created requisitionLineItem
   */
  @RequestMapping(value = "/requisitionLineItems", method = RequestMethod.POST)
  public ResponseEntity<?> createRequisitionLineItem(
      @RequestBody RequisitionLineItem requisitionLineItem) {
    try {
      LOGGER.debug("Creating new requisitionLineItem");
      requisitionLineItem.setId(null);
      RequisitionLineItem newRequisitionLineItem =
          requisitionLineItemRepository.save(requisitionLineItem);
      LOGGER.debug("Created new requisitionLineItem with id: " + requisitionLineItem.getId());
      return new ResponseEntity<>(newRequisitionLineItem, HttpStatus.CREATED);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error occurred while creating requisitionLineItem",
                ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get all requisitionLineItems.
   *
   * @return RequisitionLineItems.
   */
  @RequestMapping(value = "/requisitionLineItems", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllRequisitionLineItems() {
    Iterable<RequisitionLineItem> requisitionLineItems = requisitionLineItemRepository.findAll();
    return new ResponseEntity<>(requisitionLineItems, HttpStatus.OK);
  }

  /**
   * Allows updating requisitionLineItems.
   *
   * @param requisitionLineItem A requisitionLineItem bound to the request body
   * @param requisitionLineItemId UUID of requisitionLineItem which we want to update
   * @return ResponseEntity containing the updated requisitionLineItem
   */
  @RequestMapping(value = "/requisitionLineItems/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateRequisitionLineItems(
      @RequestBody RequisitionLineItem requisitionLineItem,
      @PathVariable("id") UUID requisitionLineItemId) {

    RequisitionLineItem requisitionLineItemToUpdate =
          requisitionLineItemRepository.findOne(requisitionLineItemId);
    try {
      if (requisitionLineItemToUpdate == null) {
        requisitionLineItemToUpdate = new RequisitionLineItem();
        LOGGER.info("Creating new requisitionLineItem");
        requisitionLineItemToUpdate.updateFrom(requisitionLineItem);
        requisitionLineItemToUpdate =
            requisitionLineItemRepository.save(requisitionLineItemToUpdate);
      } else {

        LOGGER.debug("Updating requisitionLineItem with id: " + requisitionLineItemId);

        if (requisitionLineItemToUpdate.getRequisition().getStatus() == RequisitionStatus.INITIATED
              || requisitionLineItemToUpdate.getRequisition().getStatus()
              == RequisitionStatus.SUBMITTED) {
          requisitionLineItemToUpdate.updateFrom(requisitionLineItem);
          requisitionLineItemToUpdate =
              requisitionLineItemRepository.save(requisitionLineItemToUpdate);
        } else if (requisitionLineItemToUpdate.getRequisition().getStatus()
              == RequisitionStatus.AUTHORIZED) {
          requisitionLineItemToUpdate.setApprovedQuantity(
              requisitionLineItem.getApprovedQuantity());
          requisitionLineItemToUpdate.setRemarks(requisitionLineItem.getRemarks());
          requisitionLineItemToUpdate =
              requisitionLineItemRepository.save(requisitionLineItemToUpdate);
        }
      }
      LOGGER.debug("Saved requisitionLineItem with id: " + requisitionLineItemToUpdate.getId());
      return new ResponseEntity<RequisitionLineItem>(requisitionLineItemToUpdate, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error accurred while saving requisitionLineItem with id: "
                  + requisitionLineItemToUpdate.getId(), ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get chosen requisitionLineItem.
   *
   * @param requisitionLineItemId UUID of requisitionLineItem which we want to get
   * @return RequisitionLineItem.
   */
  @RequestMapping(value = "/requisitionLineItems/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getRequisitionLineItem(@PathVariable("id") UUID requisitionLineItemId) {
    RequisitionLineItem requisitionLineItem =
        requisitionLineItemRepository.findOne(requisitionLineItemId);
    if (requisitionLineItem == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(requisitionLineItem, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting requisitionLineItem.
   *
   * @param requisitionLineItemId UUID of requisitionLineItem which we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/requisitionLineItems/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteRequisitionLineItem(
      @PathVariable("id") UUID requisitionLineItemId) {
    RequisitionLineItem requisitionLineItem =
        requisitionLineItemRepository.findOne(requisitionLineItemId);
    if (requisitionLineItem == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        requisitionLineItemRepository.delete(requisitionLineItem);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("An error accurred while deleting requisitionLineItem with id: "
                    + requisitionLineItemId, ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<RequisitionLineItem>(HttpStatus.NO_CONTENT);
    }
  }

  /**
   * Returns all requisition lines with matched parameters.
   * @param requisition requisition of searched requisition lines.
   * @param product product of searched requisition lines.
   * @return ResponseEntity with list of all requisition lines matching
   *         provided parameters and OK httpStatus.
   */
  @RequestMapping(value = "/requisitionLineItems/search", method = RequestMethod.GET)
  public ResponseEntity<?> searchRequisitionLineItems(
      @RequestParam(value = "requisition", required = true) Requisition requisition,
      @RequestParam(value = "product", required = true) UUID product) {
    List<RequisitionLineItem> result = requisitionLineItemService
        .searchRequisitionLineItems(requisition, product);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }
}
