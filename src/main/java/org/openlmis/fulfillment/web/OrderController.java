package org.openlmis.fulfillment.web;

import org.apache.tomcat.util.http.fileupload.IOUtils;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.service.OrderService;
import org.openlmis.hierarchyandsupervision.utils.ErrorResponse;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.web.BaseController;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.UserDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.oauth2.provider.OAuth2Authentication;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.UUID;

@Controller
public class OrderController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(OrderController.class);

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private OrderService orderService;

  /**
   * Allows creating new orders.
   * If the id is specified, it will be ignored.
   *
   * @param order A order bound to the request body
   * @return ResponseEntity containing the created order
   */
  @RequestMapping(value = "/orders", method = RequestMethod.POST)
  public ResponseEntity<?> createOrder(@RequestBody Order order) {
    try {
      LOGGER.debug("Creating new order");
      order.setId(null);
      Order newOrder = orderRepository.save(order);
      LOGGER.debug("Created new order with id: " + order.getId());
      return new ResponseEntity<Order>(newOrder, HttpStatus.CREATED);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error occurred while saving order", ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get all orders.
   *
   * @return Orders.
   */
  @RequestMapping(value = "/orders", method = RequestMethod.GET)
  @ResponseBody
  public ResponseEntity<?> getAllOrders() {
    Iterable<Order> orders = orderRepository.findAll();
    return new ResponseEntity<>(orders, HttpStatus.OK);
  }

  /**
   * Allows updating orders.
   *
   * @param order A order bound to the request body
   * @param orderId UUID of order which we want to update
   * @return ResponseEntity containing the updated order
   */
  @RequestMapping(value = "/orders/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateOrder(@RequestBody Order order,
                                       @PathVariable("id") UUID orderId) {

    Order orderToUpdate = orderRepository.findOne(orderId);
    try {
      if (orderToUpdate == null) {
        orderToUpdate = new Order();
        LOGGER.info("Creating new order");
      } else {
        LOGGER.debug("Updating order with id: " + orderId);
      }

      orderToUpdate.updateFrom(order);
      orderToUpdate = orderRepository.save(orderToUpdate);

      LOGGER.debug("Saved order with id: " + orderToUpdate.getId());
      return new ResponseEntity<Order>(orderToUpdate, HttpStatus.OK);
    } catch (DataIntegrityViolationException ex) {
      ErrorResponse errorResponse =
            new ErrorResponse("An error occurred while saving order with id: "
                  + orderToUpdate.getId(), ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity(HttpStatus.BAD_REQUEST);
    }
  }

  /**
   * Get chosen order.
   *
   * @param orderId UUID of order whose we want to get
   * @return Order.
   */
  @RequestMapping(value = "/orders/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getOrder(@PathVariable("id") UUID orderId) {
    Order order = orderRepository.findOne(orderId);
    if (order == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(order, HttpStatus.OK);
    }
  }

  /**
   * Allows deleting order.
   *
   * @param orderId UUID of order which we want to delete
   * @return ResponseEntity containing the HTTP Status
   */
  @RequestMapping(value = "/orders/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteOrder(@PathVariable("id") UUID orderId) {
    Order order = orderRepository.findOne(orderId);
    if (order == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        orderRepository.delete(order);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse =
              new ErrorResponse("An error occurred while deleting order with id: "
                    + orderId, ex.getMessage());
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
      return new ResponseEntity<Order>(HttpStatus.NO_CONTENT);
    }
  }

  /**
   * Finds Orders matching all of provided parameters.
   * @param supplyingFacility supplyingFacility of searched Orders.
   * @param requestingFacility requestingFacility of searched Orders.
   * @param program program of searched Orders.
   * @return ResponseEntity with list of all Orders matching
   *         provided parameters and OK httpStatus.
   */
  @RequestMapping(value = "/orders/search", method = RequestMethod.GET)
  public ResponseEntity<Iterable<Order>> searchOrders(
          @RequestParam(value = "supplyingFacility", required = true) Facility supplyingFacility,
          @RequestParam(value = "requestingFacility", required = false) Facility requestingFacility,
          @RequestParam(value = "program", required = false) Program program) {

    List<Order> result = orderService.searchOrders(supplyingFacility, requestingFacility, program);

    return new ResponseEntity<>(result, HttpStatus.OK);
  }

  /**
   * Allows finalizing orders.
   *
   * @param orderId The UUID of the order to finalize
   * @return ResponseEntity with the "#200 OK" HTTP response status on success
  or ResponseEntity containing the error description and "#400 Bad Request" status
   */
  @RequestMapping(value = "/orders/{id}/finalize", method = RequestMethod.PUT)
  public ResponseEntity<?> finalize(@PathVariable("id") UUID orderId) {

    Order order = orderRepository.findOne(orderId);

    if (order == null || order.getStatus() != OrderStatus.ORDERED) {
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }

    LOGGER.debug("Finalizing the order");
    order.setStatus(OrderStatus.SHIPPED);
    orderRepository.save(order);

    return new ResponseEntity<>(HttpStatus.OK);
  }

  /**
   * Returns csv or pdf of defined object in response.
   *
   * @param orderId UUID of order to print
   * @param format String describing return format (pdf or csv)
   * @param response HttpServletResponse object
   */
  @RequestMapping(value = "/orders/{id}/print", method = RequestMethod.GET)
  @ResponseBody
  public void printOrder(@PathVariable("id") UUID orderId,
                         @RequestParam("format") String format,
                         HttpServletResponse response) {
    Order order = orderRepository.findOne(orderId);
    if (order == null) {
      try {
        response.sendError(HttpServletResponse.SC_BAD_REQUEST, "Order does not exist.");
      } catch (IOException ex) {
        LOGGER.info("Error sending error message to client.", ex);
      }
    }
    String[] columns = {"productName", "filledQuantity", "orderedQuantity"};
    if (format.equals("pdf")) {
      response.setContentType("application/pdf");
      response.addHeader("Content-Disposition",
              "attachment; filename=order-" + order.getOrderCode() + ".pdf");
      try {
        orderService.orderToPdf(order, columns, response.getOutputStream());
      } catch (IOException ex) {
        LOGGER.debug("Error getting response output stream.", ex);
      }
    } else {
      response.setContentType("text/csv");
      response.addHeader("Content-Disposition",
              "attachment; filename=order" + order.getOrderCode() + ".csv");
      String csvContent = orderService.orderToCsv(order, columns);
      try {
        InputStream input = new ByteArrayInputStream(csvContent.getBytes(StandardCharsets.UTF_8));
        IOUtils.copy(input, response.getOutputStream());
        response.flushBuffer();
      } catch (IOException ex) {
        LOGGER.debug("Error writing csv file to output stream.", ex);
      }
    }
  }

  /**
   * Converting Requisition list to orders.
   *
   * @param requisitionList List of Requisitions that will be converted to Orders
   * @return ResponseEntity with the "#200 OK" HTTP response status on success
   */
  @RequestMapping(value = "/orders/requisitions", method = RequestMethod.POST)
  public ResponseEntity<?> convertToOrder(@RequestBody List<Requisition> requisitionList,
                                          OAuth2Authentication auth) {
    UUID userId = null;
    if (auth != null && auth.getPrincipal() != null) {
      userId = ((UserDto) auth.getPrincipal()).getId();
    }
    orderService.convertToOrder(requisitionList, userId);
    return new ResponseEntity<>(HttpStatus.CREATED);
  }
}
