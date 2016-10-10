package org.openlmis.requisition.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLineItem;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.openlmis.fulfillment.domain.ProofOfDeliveryLineItem;
import org.openlmis.fulfillment.repository.OrderLineItemRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryLineItemRepository;
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
public class ProofOfDeliveryLineItemRepositoryIntegrationTest {

  @Autowired
  private OrderLineItemRepository orderLineItemRepository;

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private ProofOfDeliveryRepository proofOfDeliveryRepository;

  @Autowired
  private ProofOfDeliveryLineItemRepository proofOfDeliveryLineItemRepository;

  private static final String CODE = "ProofOfDeliveryLineItemRepositoryIntegrationTest";

  private OrderLineItem orderLineItem = new OrderLineItem();

  private ProofOfDelivery proofOfDelivery = new ProofOfDelivery();

  @Before
  public void setUp() {
    Order order = new Order();
    order.setOrderCode(CODE);
    order.setQuotedCost(new BigDecimal("1.29"));
    order.setStatus(OrderStatus.PICKING);
    order.setProgramId(UUID.randomUUID());
    order.setCreatedById(UUID.randomUUID());
    order.setRequestingFacilityId(UUID.randomUUID());
    order.setReceivingFacilityId(UUID.randomUUID());
    order.setSupplyingFacilityId(UUID.randomUUID());
    orderRepository.save(order);

    orderLineItem.setOrder(order);
    orderLineItem.setOrderableProductId(UUID.randomUUID());
    orderLineItem.setOrderedQuantity(5L);
    orderLineItem.setFilledQuantity(5L);
    orderLineItemRepository.save(orderLineItem);

    proofOfDelivery.setOrder(order);
    proofOfDelivery.setDeliveredBy(CODE);
    proofOfDelivery.setReceivedBy(CODE);
    proofOfDelivery.setReceivedDate(LocalDate.now());
    proofOfDeliveryRepository.save(proofOfDelivery);
  }

  @Test
  public void testCreate() {
    ProofOfDeliveryLineItem proofOfDeliveryLineItem = new ProofOfDeliveryLineItem();
    proofOfDeliveryLineItem.setOrderLineItem(orderLineItem);
    proofOfDeliveryLineItem.setProofOfDelivery(proofOfDelivery);

    Assert.assertNull(proofOfDeliveryLineItem.getId());

    proofOfDeliveryLineItem =
        proofOfDeliveryLineItemRepository.save(proofOfDeliveryLineItem);
    Assert.assertNotNull(proofOfDeliveryLineItem.getId());
  }
}
