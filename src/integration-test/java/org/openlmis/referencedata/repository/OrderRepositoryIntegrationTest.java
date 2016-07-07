package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Program;

import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class OrderRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<Order> {

  @Autowired
  OrderRepository repository;

  @Autowired
  ProgramRepository programRepository;

  @Autowired
  UserRepository userRepository;

  @Autowired
  FacilityRepository facilityRepository;

  OrderRepository getRepository() {
    return this.repository;
  }

  private String[] orderRepository = {"OrderRepositoryIntegrationTest1", "OrderRepositoryIntegrationTest2", "OrderRepositoryIntegrationTest3", "OrderRepositoryIntegrationTest4", "OrderRepositoryIntegrationTest5"};
  private List<Facility> testFacilities;
  private List<Program> testPrograms;
  private int testSetSize = 51;

  private Facility facility = new Facility();
  private Program program = new Program();
  private User user = new User();

  /** Prepare the test environment. */
  @Before
  public void setUp() {

    testFacilities = new ArrayList<>();
    testPrograms = new ArrayList<>();

    facilityRepository.deleteAll();
    FacilityType facilityType = new FacilityType();
    for ( int i = 0; i < orderRepository.length; i++) {
      facility = new Facility();
      facilityType.setCode(orderRepository[i]);

      GeographicLevel level = new GeographicLevel();
      level.setCode(orderRepository[i]);
      level.setLevelNumber(1);

      GeographicZone geographicZone = new GeographicZone();
      geographicZone.setCode(orderRepository[i]);
      geographicZone.setLevel(level);

      facility.setType(facilityType);
      facility.setGeographicZone(geographicZone);
      facility.setCode(orderRepository[i]);
      facility.setName(orderRepository[i]);
      facility.setDescription("Test facility");
      facility.setActive(true);
      facility.setEnabled(true);
      facilityRepository.save(facility);
      testFacilities.add(facility);
    }

    programRepository.deleteAll();
    for ( int i = 0; i < orderRepository.length;i++) {
      program.setCode(orderRepository[i]);
      programRepository.save(program);
      testPrograms.add(program);
    }

    userRepository.deleteAll();
    user.setUsername(orderRepository[0]);
    user.setPassword(orderRepository[0]);
    user.setFirstName("Test");
    user.setLastName("User");
    userRepository.save(user);

  }

  Order generateInstance() {
    int instanceNumber = this.getNextInstanceNumber();
    Order order = new Order();
    order.setOrderCode("O" + instanceNumber);
    order.setQuotedCost(new BigDecimal("1.29"));
    order.setStatus(OrderStatus.PICKING);
    order.setProgram(program);
    order.setCreatedBy(user);
    order.setRequestingFacility(facility);
    order.setReceivingFacility(facility);
    order.setSupplyingFacility(facility);
    return order;
  }

  @Test
  public void testFindBySupplyingFacility() {
    generateTestSet();
    for (int i = 0; i < orderRepository.length; i++) {
      Iterable<Order> result = repository.findBySupplyingFacility(testFacilities.get(i));

      Iterator iterator = result.iterator();
      while (iterator.hasNext()) {
        Order order = (Order)iterator.next();
        Assert.assertEquals(order.getSupplyingFacility(), testFacilities.get(i));
      }
    }
  }

  @Test
  public void testFindBySupplyingFacilityAndRequestingFacility() {
    generateTestSet();
    for (int i = 0; i < orderRepository.length; i++) {
      Iterable<Order> result = repository.findBySupplyingFacilityAndRequestingFacility(testFacilities.get(i), testFacilities.get(i));

      Iterator iterator = result.iterator();
      while (iterator.hasNext()) {
        Order order = (Order)iterator.next();
        Assert.assertEquals(order.getSupplyingFacility(), testFacilities.get(i));
        Assert.assertEquals(order.getRequestingFacility(), testFacilities.get(i));
      }
    }
  }

  @Test
  public void testFindBySupplyingFacilityAndProgram() {
    generateTestSet();
    for (int i = 0; i < orderRepository.length; i++) {
      Iterable<Order> result = repository.findBySupplyingFacilityAndProgram(testFacilities.get(i), testPrograms.get(i));

      Iterator iterator = result.iterator();
      while (iterator.hasNext()) {
        Order order = (Order)iterator.next();
        Assert.assertEquals(order.getSupplyingFacility(), testFacilities.get(i));
        Assert.assertEquals(order.getProgram(), testPrograms.get(i));
      }
    }
  }

  @Test
  public void testFindBySupplyingFacilityAndRequestingFacilityAndProgram() {
    generateTestSet();
    for ( int i = 0; i < orderRepository.length; i++) {
      Iterable<Order> result = repository.findBySupplyingFacilityAndRequestingFacilityAndProgram(testFacilities.get(i), testFacilities.get(i), testPrograms.get(i));

      Iterator iterator = result.iterator();
      while (iterator.hasNext()) {
        Order order = (Order)iterator.next();
        Assert.assertEquals(order.getSupplyingFacility(), testFacilities.get(i));
        Assert.assertEquals(order.getRequestingFacility(), testFacilities.get(i));
        Assert.assertEquals(order.getProgram(), testPrograms.get(i));
      }
    }
  }


  private void generateTestSet() {
    for (int i = 0; i < testSetSize; i++) {
      Order tmp = generateInstance();
      tmp.setSupplyingFacility(testFacilities.get(i % orderRepository.length));
      tmp.setRequestingFacility(testFacilities.get(i % orderRepository.length));
      tmp.setProgram(testPrograms.get(i % orderRepository.length));
      repository.save(tmp);
    }
  }
}
