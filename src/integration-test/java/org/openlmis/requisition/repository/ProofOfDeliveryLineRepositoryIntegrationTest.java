package org.openlmis.requisition.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.openlmis.fulfillment.domain.ProofOfDeliveryLine;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryLineRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
public class ProofOfDeliveryLineRepositoryIntegrationTest {

  @Autowired
  private OrderLineRepository orderLineRepository;

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private ProofOfDeliveryRepository proofOfDeliveryRepository;

  @Autowired
  private ProofOfDeliveryLineRepository proofOfDeliveryLineRepository;

  private static final String CODE = "ProofOfDeliveryLineRepositoryIntegrationTest";

  private OrderLine orderLine = new OrderLine();

  private ProofOfDelivery proofOfDelivery = new ProofOfDelivery();

  @Before
  public void setUp() {
    Order order = new Order();
    order.setOrderCode(CODE);
    order.setQuotedCost(new BigDecimal("1.29"));
    order.setStatus(OrderStatus.PICKING);
    order.setProgram(UUID.randomUUID());
    order.setCreatedById(UUID.randomUUID());
    order.setRequestingFacility(UUID.randomUUID());
    order.setReceivingFacility(UUID.randomUUID());
    order.setSupplyingFacility(UUID.randomUUID());
    orderRepository.save(order);

    orderLine.setOrder(order);
    orderLine.setProduct(UUID.randomUUID());
    orderLine.setOrderedQuantity(5L);
    orderLine.setFilledQuantity(5L);
    orderLineRepository.save(orderLine);

    proofOfDelivery.setOrder(order);
    proofOfDelivery.setDeliveredBy(CODE);
    proofOfDelivery.setReceivedBy(CODE);
    proofOfDelivery.setReceivedDate(LocalDate.now());
    proofOfDeliveryRepository.save(proofOfDelivery);
  }

  @Test
  public void testCreate() {
    ProofOfDeliveryLine proofOfDeliveryLine = new ProofOfDeliveryLine();
    proofOfDeliveryLine.setOrderLine(orderLine);
    proofOfDeliveryLine.setProofOfDelivery(proofOfDelivery);

    Assert.assertNull(proofOfDeliveryLine.getId());

    proofOfDeliveryLine =
        proofOfDeliveryLineRepository.save(proofOfDeliveryLine);
    Assert.assertNotNull(proofOfDeliveryLine.getId());
  }
}
