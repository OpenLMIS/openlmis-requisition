package org.openlmis.fulfillment.web;

import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
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

import java.util.UUID;

@RepositoryRestController
public class OrderLineController {

  private static final Logger LOGGER = LoggerFactory.getLogger(OrderController.class);

  @Autowired
  private OrderLineRepository orderLineRepository;

  /**
   * Allows creating new orderLines.
   *
   * @param orderLine A orderLine bound to the request body
   * @return ResponseEntity containing the created orderLine
   */
  @RequestMapping(value = "/orderLines", method = RequestMethod.POST)
  public ResponseEntity<?> createOrderLine(@RequestBody OrderLine orderLine) {
    if (orderLine == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      LOGGER.debug("Creating new orderLine");
      // Ignore provided id
      orderLine.setId(null);
      OrderLine newOrderLine = orderLineRepository.save(orderLine);
      return new ResponseEntity<OrderLine>(newOrderLine, HttpStatus.CREATED);
    }
  }

  /**
   * Get all orderLines.
   *
   * @return OrderLines.
   */
  @RequestMapping(value = "/orderLines", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllOrderLines() {
    Iterable<OrderLine> orderLines = orderLineRepository.findAll();
    if (orderLines == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(orderLines, HttpStatus.OK);
    }
  }

  /**
   * Allows updating orderLines.
   *
   * @param orderLine A orderLine bound to the request body
   * @param orderLineId UUID of orderLine which we want to update
   * @return ResponseEntity containing the updated orderLine
   */
  @RequestMapping(value = "/orderLines/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateOrderLine(@RequestBody OrderLine orderLine,
                                       @PathVariable("id") UUID orderLineId) {
    OrderLine orderLineFromDb = orderLineRepository.findOne(orderLineId);
    if (orderLineFromDb == null) {
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    } else {
      LOGGER.debug("Updating order");
      OrderLine updatedOrderLine = orderLineRepository.save(orderLine);
      return new ResponseEntity<OrderLine>(updatedOrderLine, HttpStatus.OK);
    }
  }

  /**
   * Get chosen orderLine.
   *
   * @param orderLineId UUID of orderLine whose we want to get
   * @return OrderLine.
   */
  @RequestMapping(value = "/orderLines/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getOrderLine(@PathVariable("id") UUID orderLineId) {
    OrderLine orderLine = orderLineRepository.findOne(orderLineId);
    if (orderLine == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(orderLine, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting orderLine.
   *
   * @param orderLineId UUID of orderLine whose we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/orderLines/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteOrderLine(@PathVariable("id") UUID orderLineId) {
    OrderLine orderLine = orderLineRepository.findOne(orderLineId);
    if (orderLine == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        orderLineRepository.delete(orderLine);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("OrderLine cannot be deleted because of existing dependencies",
                    ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<OrderLine>(HttpStatus.NO_CONTENT);
    }
  }
}
