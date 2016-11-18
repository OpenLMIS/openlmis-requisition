package org.openlmis.requisition.service.order;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.service.InternalOrderService;
import org.openlmis.requisition.dto.OrderDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class OrderService {

  @Autowired
  private InternalOrderService internalOrderService;

  /**
   * Saves a new instance of order.
   *
   * @param orderDto instance that contain data required to save order
   */
  public void save(OrderDto orderDto) {
    internalOrderService.save(Order.newOrder(orderDto));
  }
}
