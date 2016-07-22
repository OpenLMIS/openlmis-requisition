package org.openlmis.csv.web;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.service.OrderService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@RepositoryRestController
public class CsvController {

  private Logger logger = LoggerFactory.getLogger(CsvController.class);

  @Autowired
  private OrderRepository orderRepository;

  /**
   * Get CSV from Order.
   */
  @RequestMapping(value = "/orders/csv/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getCsv(@PathVariable("id") UUID orderId) {
    Order order = orderRepository.findOne(orderId);
    if (order == null) {
      logger.debug("Can't find order with id: {} !", orderId);
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }

    List<String> header = new ArrayList<>();
    header.add(OrderService.DEFAULT_COLUMNS[0]);
    header.add(OrderService.DEFAULT_COLUMNS[1]);
    header.add(OrderService.DEFAULT_COLUMNS[2]);
    header.add(OrderService.DEFAULT_COLUMNS[3]);
    header.add(OrderService.DEFAULT_COLUMNS[4]);
    header.add(OrderService.DEFAULT_COLUMNS[5]);

    OrderService generator = new OrderService();
    String csv = generator.orderToCsv(order, header.toArray(new String[0]));
    return new ResponseEntity<Object>(csv, HttpStatus.OK);
  }
}
