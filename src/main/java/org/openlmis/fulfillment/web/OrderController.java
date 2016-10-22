package org.openlmis.fulfillment.web;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderFileTemplate;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.exception.OrderCsvWriteException;
import org.openlmis.fulfillment.exception.OrderPdfWriteException;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.service.OrderFileTemplateService;
import org.openlmis.fulfillment.service.OrderService;
import org.openlmis.fulfillment.utils.OrderCsvHelper;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.requisition.web.BaseController;
import org.openlmis.utils.ErrorResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.oauth2.provider.OAuth2Authentication;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.servlet.http.HttpServletResponse;

@Controller
public class OrderController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(OrderController.class);

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private OrderService orderService;

  @Autowired
  private OrderCsvHelper csvHelper;

  @Autowired
  private OrderFileTemplateService orderFileTemplateService;

  @Autowired
  private UserReferenceDataService userReferenceDataService;

  /**
   * Allows creating new orders.
   * If the id is specified, it will be ignored.
   *
   * @param order A order bound to the request body
   * @return ResponseEntity containing the created order
   */
  @RequestMapping(value = "/orders", method = RequestMethod.POST)
  @ResponseStatus(HttpStatus.CREATED)
  @ResponseBody
  public Order createOrder(@RequestBody Order order) {
    LOGGER.debug("Creating new order");
    order.setId(null);
    Order newOrder = orderRepository.save(order);
    LOGGER.debug("Created new order with id: {}", order.getId());
    return newOrder;
  }

  /**
   * Get all orders.
   *
   * @return Orders.
   */
  @RequestMapping(value = "/orders", method = RequestMethod.GET)
  @ResponseBody
  public Iterable<Order> getAllOrders() {
    return orderRepository.findAll();
  }

  /**
   * Allows updating orders.
   *
   * @param order A order bound to the request body
   * @param orderId UUID of order which we want to update
   * @return ResponseEntity containing the updated order
   */
  @RequestMapping(value = "/orders/{id}", method = RequestMethod.PUT)
  @ResponseBody
  public Order updateOrder(@RequestBody Order order,
                                       @PathVariable("id") UUID orderId) {

    Order orderToUpdate = orderRepository.findOne(orderId);
    if (orderToUpdate == null) {
      orderToUpdate = new Order();
      LOGGER.info("Creating new order");
    } else {
      LOGGER.debug("Updating order with id: {}", orderId);
    }

    orderToUpdate.updateFrom(order);
    orderToUpdate = orderRepository.save(orderToUpdate);

    LOGGER.debug("Saved order with id: {}", orderToUpdate.getId());

    return orderToUpdate;
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
      orderRepository.delete(order);
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
  @ResponseBody
  public Iterable<Order> searchOrders(
          @RequestParam(value = "supplyingFacility", required = true) UUID supplyingFacility,
          @RequestParam(value = "requestingFacility", required = false)
              UUID requestingFacility,
          @RequestParam(value = "program", required = false) UUID program) {

    return orderService.searchOrders(supplyingFacility, requestingFacility, program);
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

    LOGGER.debug("Finalizing the order with id: {}", order);
    order.setStatus(OrderStatus.SHIPPED);
    orderRepository.save(order);

    return new ResponseEntity<>(HttpStatus.OK);
  }

  /**
   * Converting Requisition list to orders.
   *
   * @param requisitionIdList List of UUIDs of Requisitions that will be converted to Orders
   * @return ResponseEntity with the "#200 OK" HTTP response status on success
   */
  @RequestMapping(value = "/orders/requisitions", method = RequestMethod.POST)
  public ResponseEntity<?> convertToOrder(@RequestBody List<UUID> requisitionIdList,
                                          OAuth2Authentication auth) {
    UUID userId = null;

    if (auth != null && auth.getPrincipal() != null) {
      String username =
          (String) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

      Map<String, Object> parameters = new HashMap<>();
      parameters.put("username", username);

      List<UserDto> users =
          new ArrayList<>(userReferenceDataService.findUsers(parameters));

      userId = users.get(0).getId();
    }
    try {
      orderService.convertToOrder(requisitionIdList, userId);
    } catch (RequisitionException ex) {
      ErrorResponse errorResponse =
              new ErrorResponse("An error occurred while converting requisitions to order",
                      ex.getMessage());
      LOGGER.error(errorResponse.getMessage(), ex);
      return new ResponseEntity<>(errorResponse, HttpStatus.BAD_REQUEST);
    }
    return new ResponseEntity<>(HttpStatus.CREATED);
  }

  /**
   * Returns csv or pdf of defined object in response.
   *
   * @param orderId UUID of order to print
   * @param format String describing return format (pdf or csv)
   * @param response HttpServletResponse object
   */
  @RequestMapping(value = "/orders/{id}/print", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  public void printOrder(@PathVariable("id") UUID orderId,
                         @RequestParam("format") String format,
                         HttpServletResponse response) throws IOException,
          OrderCsvWriteException, OrderPdfWriteException {

    Order order = orderRepository.findOne(orderId);
    if (order == null) {
      response.sendError(HttpServletResponse.SC_BAD_REQUEST, "Order does not exist.");
      return;
    }

    String[] columns = {"productName", "filledQuantity", "orderedQuantity"};
    if (format.equals("pdf")) {
      response.setContentType("application/pdf");
      response.addHeader("Content-Disposition",
              "attachment; filename=order-" + order.getOrderCode() + ".pdf");
      orderService.orderToPdf(order, columns, response.getOutputStream());
    } else {
      response.setContentType("text/csv");
      response.addHeader("Content-Disposition",
              "attachment; filename=order" + order.getOrderCode() + ".csv");
      orderService.orderToCsv(order, columns, response.getWriter());
    }
  }

  /**
   * Exporting order to csv.
   *
   * @param orderId UUID of order to print
   * @param response HttpServletResponse object
   */
  @RequestMapping(value = "/orders/{id}/csv", method = RequestMethod.GET)
  @ResponseStatus(HttpStatus.OK)
  public void exportToCsv(@PathVariable("id") UUID orderId, HttpServletResponse response)
          throws IOException {
    Order order = orderRepository.findOne(orderId);
    OrderFileTemplate orderFileTemplate = orderFileTemplateService.getOrderFileTemplate();

    if (order == null) {
      response.sendError(HttpServletResponse.SC_NOT_FOUND, "Order does not exist.");
      return;
    }

    if (orderFileTemplate == null) {
      response.sendError(HttpServletResponse.SC_NOT_FOUND,
          "Could not export Order, because Order Template File not found");
      return;
    }

    response.setContentType("text/csv");
    response.addHeader("Content-Disposition", "attachment; filename="
        + orderFileTemplate.getFilePrefix() + order.getOrderCode() + ".csv");

    try {
      csvHelper.writeCsvFile(order, orderFileTemplate, response.getWriter());
    } catch (IOException ex) {
      response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
          "Error occurred while exporting order to csv.");
      LOGGER.error("Error occurred while exporting order to csv", ex);
    }
  }
}
