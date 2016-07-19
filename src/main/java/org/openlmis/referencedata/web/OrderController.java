package org.openlmis.referencedata.web;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.referencedata.domain.Stock;
import org.openlmis.referencedata.repository.StockRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.util.UUID;

@RepositoryRestController
public class OrderController {
  private Logger logger = LoggerFactory.getLogger(OrderController.class);

  @Autowired
  private StockRepository stockRepository;

  @Autowired
  private OrderRepository orderRepository;

  /**
   * Allows finalizing orders.
   *
   * @param orderId The UUID of the order to finalize
   * @return ResponseEntity with the "#200 OK" HTTP response status on success
  or ResponseEntity containing the error description and "#400 Bad Request" status
   */

  @RequestMapping(value = "/orders/finalizeOrder", method = RequestMethod.POST)
  public ResponseEntity<?> finalizeOrder(@RequestBody UUID orderId) {

    Order order = orderRepository.findOne(orderId);

    if (order == null) {
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    } else {
      for (OrderLine orderLine : order.getOrderLines()) {
        //Searching for a corresponding stock to the current orderline
        Stock stock = stockRepository.findByStockInventoryAndProduct(
            order.getSupplyingFacility().getStockInventory(),
            orderLine.getProduct()
        );
        //Checking if the stock exists.
        String productName = orderLine.getProduct().getPrimaryName();
        if (stock == null) {
          return new ResponseEntity<>(
              "Error: There is no " + productName + " in the stock inventory.",
              HttpStatus.BAD_REQUEST
          );
        }
        //Checking if there is sufficient quanitity of the ordered products.
        if (stock.getStoredQuantity() < orderLine.getOrderedQuantity()) {
          return new ResponseEntity<>(
              "Error: There is insufficient quantity of " + productName
                  + " in the stock inventory.",
              HttpStatus.BAD_REQUEST
          );
        }
      }

      logger.debug("Finalizing the order");

      /*Once finalized has been selected all commodities are subtracted
        from inventory at the warehouse*/
      for (OrderLine orderLine : order.getOrderLines()) {
        Stock stock = stockRepository.findByStockInventoryAndProduct(
            order.getSupplyingFacility().getStockInventory(),
            orderLine.getProduct()
        );
        stock.setStoredQuantity(
            stock.getStoredQuantity() - orderLine.getOrderedQuantity()
        );
        stockRepository.save(stock);
      }

      order.setStatus(OrderStatus.SHIPPED);
      orderRepository.save(order);

      return new ResponseEntity<>(HttpStatus.OK);
    }
  }
}
