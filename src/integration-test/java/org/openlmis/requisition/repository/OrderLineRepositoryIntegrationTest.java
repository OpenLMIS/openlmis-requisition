package org.openlmis.requisition.repository;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
public class OrderLineRepositoryIntegrationTest {

  private static final String orderLine = "OrderLineRepositoryIntegrationTest";

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private OrderLineRepository orderLineRepository;

  private Order order = new Order();
  private UUID userId = UUID.randomUUID();
  private UUID programId = UUID.randomUUID();
  private UUID requestingFacilityId = UUID.randomUUID();
  private UUID receivingFacilityId = UUID.randomUUID();
  private UUID supplyingFacilityId = UUID.randomUUID();

  @Before
  public void setUp() {
    order.setOrderCode(orderLine);
    order.setQuotedCost(new BigDecimal("1.29"));
    order.setStatus(OrderStatus.PICKING);
    order.setProgram(programId);
    order.setCreatedById(userId);
    order.setRequestingFacility(requestingFacilityId);
    order.setReceivingFacility(receivingFacilityId);
    order.setSupplyingFacility(supplyingFacilityId);
    orderRepository.save(order);
  }

  @Test
  public void testCreate() {
    OrderLine orderLine = new OrderLine();
    orderLine.setOrder(order);
    orderLine.setOrderedQuantity(5L);

    assertNull(orderLine.getId());

    orderLine = orderLineRepository.save(orderLine);
    assertNotNull(orderLine.getId());
  }
}
