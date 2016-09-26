package org.openlmis.fulfillment.web;

import org.openlmis.fulfillment.domain.ProofOfDeliveryLineItem;
import org.openlmis.fulfillment.repository.ProofOfDeliveryLineItemRepository;
import org.openlmis.requisition.web.BaseController;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
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
public class ProofOfDeliveryLineItemController extends BaseController {

  private static final Logger LOGGER =
        LoggerFactory.getLogger(ProofOfDeliveryLineItemController.class);

  @Autowired
  private ProofOfDeliveryLineItemRepository proofOfDeliveryLineItemRepository;


  /**
   * Allows creating new proofOfDeliveryLineItems.
   * If the id is specified, it will be ignored.
   *
   * @param proofOfDeliveryLineItem A proofOfDeliveryLineItem bound to the request body
   * @return ResponseEntity containing the created proofOfDeliveryLineItem
   */
  @RequestMapping(value = "/proofOfDeliveryLineItems", method = RequestMethod.POST)
  public ResponseEntity<?> createProofOfDeliveryLineItem(
          @RequestBody ProofOfDeliveryLineItem proofOfDeliveryLineItem) {
    LOGGER.debug("Creating new proofOfDeliveryLineItem");
    // Ignore provided id
    proofOfDeliveryLineItem.setId(null);
    ProofOfDeliveryLineItem newProofOfDeliveryLineItem
            = proofOfDeliveryLineItemRepository.save(proofOfDeliveryLineItem);
    LOGGER.debug("Created new proofOfDeliveryLineItem with id: "
        + proofOfDeliveryLineItem.getId());
    return new ResponseEntity<>(newProofOfDeliveryLineItem, HttpStatus.CREATED);
  }

  /**
   * Get all proofOfDeliveryLineItems.
   *
   * @return ProofOfDeliveryLineItem.
   */
  @RequestMapping(value = "/proofOfDeliveryLineItems", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllProofOfDeliveryLineItems() {
    Iterable<ProofOfDeliveryLineItem> proofOfDeliveryLineItems =
        proofOfDeliveryLineItemRepository.findAll();
    return new ResponseEntity<>(proofOfDeliveryLineItems, HttpStatus.OK);
  }

  /**
   * Allows updating proofOfDeliveryLineItems.
   *
   * @param proofOfDeliveryLineItem A proofOfDeliveryLineItem bound to the request body
   * @param proofOfDeliveryLineItemId UUID of proofOfDeliveryLineItem which we want to update
   * @return ResponseEntity containing the updated proofOfDeliveryLineItem
   */
  @RequestMapping(value = "/proofOfDeliveryLineItems/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateProofOfDeliveryLineItem(
        @RequestBody ProofOfDeliveryLineItem proofOfDeliveryLineItem,
        @PathVariable("id") UUID proofOfDeliveryLineItemId) {

    ProofOfDeliveryLineItem proofOfDeliveryLineItemToUpdate
          = proofOfDeliveryLineItemRepository.findOne(proofOfDeliveryLineItemId);
    if (proofOfDeliveryLineItemToUpdate == null) {
      proofOfDeliveryLineItemToUpdate = new ProofOfDeliveryLineItem();
      LOGGER.info("Creating new proofOfDeliveryLineItem");
    } else {
      LOGGER.debug("Updating proofOfDeliveryLineItem with id: " + proofOfDeliveryLineItemId);
    }

    proofOfDeliveryLineItemToUpdate.updateFrom(proofOfDeliveryLineItem);
    proofOfDeliveryLineItemToUpdate
          = proofOfDeliveryLineItemRepository.save(proofOfDeliveryLineItemToUpdate);

    LOGGER.debug("Saved proofOfDeliveryLineItem with id: "
        + proofOfDeliveryLineItemToUpdate.getId());
    return new ResponseEntity<>(proofOfDeliveryLineItemToUpdate, HttpStatus.OK);
  }

  /**
   * Get chosen proofOfDeliveryLineItem.
   *
   * @param proofOfDeliveryLineItemId UUID of proofOfDeliveryLineItem whose we want to get
   * @return ProofOfDeliveryLineItem.
   */
  @RequestMapping(value = "/proofOfDeliveryLineItems/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getProofOfDeliveryLineItem(
      @PathVariable("id") UUID proofOfDeliveryLineItemId) {
    ProofOfDeliveryLineItem proofOfDeliveryLineItem
            = proofOfDeliveryLineItemRepository.findOne(proofOfDeliveryLineItemId);
    if (proofOfDeliveryLineItem == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(proofOfDeliveryLineItem, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting proofOfDeliveryLineItem.
   *
   * @param proofOfDeliveryLineItemId UUID of proofOfDeliveryLineItem whose we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/proofOfDeliveryLineItems/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteProofOfDeliveryLineItem(
          @PathVariable("id") UUID proofOfDeliveryLineItemId) {
    ProofOfDeliveryLineItem proofOfDeliveryLineItem
            = proofOfDeliveryLineItemRepository.findOne(proofOfDeliveryLineItemId);
    if (proofOfDeliveryLineItem == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      proofOfDeliveryLineItemRepository.delete(proofOfDeliveryLineItem);
      return new ResponseEntity<ProofOfDeliveryLineItem>(HttpStatus.NO_CONTENT);
    }
  }
}
