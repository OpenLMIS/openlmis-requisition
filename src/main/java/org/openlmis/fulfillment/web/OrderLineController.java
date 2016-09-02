package org.openlmis.fulfillment.web;

import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
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
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.UUID;

@Controller
public class OrderLineController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(OrderController.class);

  @Autowired
  private OrderLineRepository orderLineRepository;

  /**
   * Allows creating new orderLines.
   * If the id is specified, it will be ignored.
   *
   * @param orderLine A orderLine bound to the request body
   * @return ResponseEntity containing the created orderLine
   */
  @RequestMapping(value = "/orderLines", method = RequestMethod.POST)
  public ResponseEntity<?> createOrderLine(@RequestBody OrderLine orderLine) {
    try {
      LOGGER.debug("Creating new orderLine");
      orderLine.setId(null);
      OrderLine newOrderLine = orderLineRepository.save(orderLine);
      LOGGER.debug("Creating new orderLine with id: " + orderLine.getId());
      return new ResponseEntity<OrderLine>(newOrderLine, HttpStatus.CREATED);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error occurred while saving orderLine", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
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
    return new ResponseEntity<>(orderLines, HttpStatus.OK);
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
    try {
      LOGGER.debug("Updating orderLine with id: " + orderLineId);

      OrderLine orderLineToUpdate = orderLineRepository.findOne(orderLineId);

      if (orderLineToUpdate == null) {
        orderLineToUpdate = new OrderLine();
      }

      orderLineToUpdate.updateFrom(orderLine);
      orderLineToUpdate = orderLineRepository.save(orderLineToUpdate);

      LOGGER.debug("Updated orderLine with id: " + orderLineId);
      return new ResponseEntity<OrderLine>(orderLineToUpdate, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error occurred while updating orderLine with id: "
                  + orderLineId, ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
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
              new ErrorResponse("An error occurred while deleting orderLine with id: "
                    + orderLineId, ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<OrderLine>(HttpStatus.NO_CONTENT);
    }
  }
}
