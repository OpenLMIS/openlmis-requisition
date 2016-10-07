package org.openlmis.requisition.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class OrderRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<Order> {

  @Autowired
  private OrderRepository repository;

  private List<Order> orders;

  OrderRepository getRepository() {
    return this.repository;
  }

  Order generateInstance() {
    int instanceNumber = this.getNextInstanceNumber();
    Order order = new Order();
    order.setOrderCode("O" + instanceNumber);
    order.setQuotedCost(new BigDecimal("1.29"));
    order.setStatus(OrderStatus.PICKING);
    order.setProgramId(UUID.randomUUID());
    order.setCreatedById(UUID.randomUUID());
    order.setRequestingFacilityId(UUID.randomUUID());
    order.setReceivingFacilityId(UUID.randomUUID());
    order.setSupplyingFacilityId(UUID.randomUUID());
    return order;
  }

  @Before
  public void setUp() {
    orders = new ArrayList<>();
    for (int orderCount = 0; orderCount < 5; orderCount++) {
      orders.add(repository.save(generateInstance()));
    }
  }

  @Test
  public void testSearchOrdersByAllParameters() {
    Order order = cloneOrder(orders.get(0));
    List<Order> receivedOrders = repository.searchOrders(
            order.getSupplyingFacilityId(),
            order.getRequestingFacilityId(),
            order.getProgramId());

    Assert.assertEquals(2, receivedOrders.size());
    for (Order receivedOrder : receivedOrders) {
      Assert.assertEquals(
              order.getSupplyingFacilityId(),
              receivedOrder.getSupplyingFacilityId());
      Assert.assertEquals(
              order.getRequestingFacilityId(),
              receivedOrder.getRequestingFacilityId());
      Assert.assertEquals(
              order.getProgramId(),
              receivedOrder.getProgramId());
    }
  }

  @Test
  public void testSearchOrdersByAllParametersNull() {
    List<Order> receivedOrders = repository.searchOrders(null, null, null);

    Assert.assertEquals(orders.size(), receivedOrders.size());
  }

  @Test
  public void testSearchOrdersBySupplyingFacilityAndProgram() {
    Order order = cloneOrder(orders.get(0));
    List<Order> receivedOrders = repository.searchOrders(
            order.getSupplyingFacilityId(),
            null,
            order.getProgramId());

    Assert.assertEquals(2, receivedOrders.size());
    for (Order receivedOrder : receivedOrders) {
      Assert.assertEquals(
              order.getSupplyingFacilityId(),
              receivedOrder.getSupplyingFacilityId());
      Assert.assertEquals(
              order.getProgramId(),
              receivedOrder.getProgramId());
    }
  }

  private Order cloneOrder(Order order) {
    Order clonedOrder = new Order();
    Integer instanceNumber = this.getNextInstanceNumber();
    clonedOrder.setSupplyingFacilityId(order.getSupplyingFacilityId());
    clonedOrder.setRequestingFacilityId(order.getRequestingFacilityId());
    clonedOrder.setReceivingFacilityId(order.getReceivingFacilityId());
    clonedOrder.setProgramId(order.getProgramId());
    clonedOrder.setOrderCode(order.getOrderCode() + instanceNumber);
    clonedOrder.setQuotedCost(order.getQuotedCost());
    clonedOrder.setStatus(order.getStatus());
    clonedOrder.setCreatedById(order.getCreatedById());
    clonedOrder.setCreatedDate(order.getCreatedDate());
    repository.save(clonedOrder);
    return  clonedOrder;
  }
}
