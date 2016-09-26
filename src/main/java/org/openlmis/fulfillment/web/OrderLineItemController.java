package org.openlmis.fulfillment.web;

import org.openlmis.fulfillment.domain.OrderLineItem;
import org.openlmis.fulfillment.repository.OrderLineItemRepository;
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
public class OrderLineItemController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(OrderLineItemController.class);

  @Autowired
  private OrderLineItemRepository orderLineItemRepository;

  /**
   * Allows creating new orderLineItems.
   * If the id is specified, it will be ignored.
   *
   * @param orderLineItem A orderLineItem bound to the request body
   * @return ResponseEntity containing the created orderLineItem
   */
  @RequestMapping(value = "/orderLineItems", method = RequestMethod.POST)
  public ResponseEntity<?> createOrderLineItem(@RequestBody OrderLineItem orderLineItem) {
    LOGGER.debug("Creating new orderLineItem");
    orderLineItem.setId(null);
    OrderLineItem newOrderLineItem = orderLineItemRepository.save(orderLineItem);
    LOGGER.debug("Creating new orderLineItem with id: " + orderLineItem.getId());
    return new ResponseEntity<>(newOrderLineItem, HttpStatus.CREATED);
  }

  /**
   * Get all orderLineItems.
   *
   * @return OrderLineItems.
   */
  @RequestMapping(value = "/orderLineItems", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllOrderLineItems() {
    Iterable<OrderLineItem> orderLineItems = orderLineItemRepository.findAll();
    return new ResponseEntity<>(orderLineItems, HttpStatus.OK);
  }

  /**
   * Allows updating orderLineItems.
   *
   * @param orderLineItem A orderLineItem bound to the request body
   * @param orderLineItemId UUID of orderLineItem which we want to update
   * @return ResponseEntity containing the updated orderLineItem
   */
  @RequestMapping(value = "/orderLineItems/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateOrderLineItem(@RequestBody OrderLineItem orderLineItem,
                                       @PathVariable("id") UUID orderLineItemId) {

    OrderLineItem orderLineItemToUpdate = orderLineItemRepository.findOne(orderLineItemId);
    if (orderLineItemToUpdate == null) {
      orderLineItemToUpdate = new OrderLineItem();
      LOGGER.info("Creating new orderLineItem");
    } else {
      LOGGER.debug("Updating orderLineItem with id: " + orderLineItemId);
    }

    orderLineItemToUpdate.updateFrom(orderLineItem);
    orderLineItemToUpdate = orderLineItemRepository.save(orderLineItemToUpdate);

    LOGGER.debug("Saved orderLineItem with id: " + orderLineItemToUpdate.getId());
    return new ResponseEntity<>(orderLineItemToUpdate, HttpStatus.OK);
  }

  /**
   * Get chosen orderLineItem.
   *
   * @param orderLineItemId UUID of orderLineItem whose we want to get
   * @return OrderLineItem.
   */
  @RequestMapping(value = "/orderLineItems/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getOrderLineItem(@PathVariable("id") UUID orderLineItemId) {
    OrderLineItem orderLineItem = orderLineItemRepository.findOne(orderLineItemId);
    if (orderLineItem == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(orderLineItem, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting orderLineItem.
   *
   * @param orderLineItemId UUID of orderLineItem whose we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/orderLineItems/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteOrderLineItem(@PathVariable("id") UUID orderLineItemId) {
    OrderLineItem orderLineItem = orderLineItemRepository.findOne(orderLineItemId);
    if (orderLineItem == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      orderLineItemRepository.delete(orderLineItem);
      return new ResponseEntity<OrderLineItem>(HttpStatus.NO_CONTENT);
    }
  }
}
