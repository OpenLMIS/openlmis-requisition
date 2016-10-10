package org.openlmis.requisition.repository;


import org.junit.Before;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.util.UUID;

public class ProofOfDeliveryRepositoryIntegrationTest extends
    BaseCrudRepositoryIntegrationTest<ProofOfDelivery> {

  @Autowired
  private ProofOfDeliveryRepository proofOfDeliveryRepository;

  @Autowired
  private OrderRepository orderRepository;

  private static final String CODE = "ProofOfDeliveryRepositoryIntegrationTest";

  private Order order = new Order();

  ProofOfDeliveryRepository getRepository() {
    return this.proofOfDeliveryRepository;
  }

  @Before
  public void setUp() {
    order.setOrderCode(CODE);
    order.setQuotedCost(new BigDecimal("1.29"));
    order.setStatus(OrderStatus.PICKING);
    order.setProgramId(UUID.randomUUID());
    order.setCreatedById(UUID.randomUUID());
    order.setRequestingFacilityId(UUID.randomUUID());
    order.setReceivingFacilityId(UUID.randomUUID());
    order.setSupplyingFacilityId(UUID.randomUUID());
    orderRepository.save(order);
  }

  ProofOfDelivery generateInstance() {
    ProofOfDelivery proofOfDelivery = new ProofOfDelivery();
    proofOfDelivery.setOrder(order);
    return proofOfDelivery;
  }
}
