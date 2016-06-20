package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.referencedata.Application;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Order;
import org.openlmis.referencedata.domain.OrderLine;
import org.openlmis.referencedata.domain.OrderStatus;
import org.openlmis.referencedata.domain.Product;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
public class OrderLineRepositoryIntegrationTest {

  @Autowired
  ProductRepository productRepository;

  @Autowired
  OrderRepository orderRepository;

  @Autowired
  OrderLineRepository orderLineRepository;

  @Autowired
  ProgramRepository programRepository;

  @Autowired
  UserRepository userRepository;

  @Autowired
  FacilityRepository facilityRepository;

  private String orderLine = "OrderLineRepositoryIntegrationTest";

  private Order order = new Order();
  private Product product = new Product();

  /** Prepare the test environment. */
  @Before
  public void setUp() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode(orderLine);

    GeographicLevel level = new GeographicLevel();
    level.setCode(orderLine);
    level.setLevelNumber(1);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(orderLine);
    geographicZone.setLevel(level);

    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(orderLine);
    facility.setName(orderLine);
    facility.setDescription("Test facility");
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    Program program = new Program();
    program.setCode(orderLine);
    programRepository.save(program);

    User user = new User();
    user.setUsername(orderLine);
    user.setPassword(orderLine);
    user.setFirstName("Test");
    user.setLastName("User");
    userRepository.save(user);

    order.setOrderCode(orderLine);
    order.setQuotedCost(new BigDecimal("1.29"));
    order.setStatus(OrderStatus.PICKING);
    order.setProgram(program);
    order.setCreatedBy(user);
    order.setRequestingFacility(facility);
    order.setReceivingFacility(facility);
    order.setSupplyingFacility(facility);
    orderRepository.save(order);

    product.setCode(orderLine);
    product.setPrimaryName("Product");
    product.setDispensingUnit("unit");
    product.setDosesPerDispensingUnit(10);
    product.setPackSize(1);
    product.setPackRoundingThreshold(0);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(true);
    product.setTracer(false);
    productRepository.save(product);
  }

  @Test
  public void testCreate() {
    OrderLine orderLine = new OrderLine();
    orderLine.setOrder(order);
    orderLine.setProduct(product);
    orderLine.setOrderedQuantity(5L);

    Assert.assertNull(orderLine.getId());

    orderLine = orderLineRepository.save(orderLine);
    Assert.assertNotNull(orderLine.getId());
  }
}
