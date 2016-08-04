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
  private OrderRepository repository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private UserRepository userRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  OrderRepository getRepository() {
    return this.repository;
  }
  
  private String[] orderRepository = {
      "OrderRepositoryIntegrationTest1", 
      "OrderRepositoryIntegrationTest2", 
      "OrderRepositoryIntegrationTest3", 
      "OrderRepositoryIntegrationTest4", 
      "OrderRepositoryIntegrationTest5"
  };
  private List<Facility> testFacilities;
  private List<Program> testPrograms;
  private int testSetSize = 51;

  private Facility facility = new Facility();
  private Program program = new Program();
  private User user;

  @Before
  public void setUp() {
    testFacilities = new ArrayList<>();
    testPrograms = new ArrayList<>();

    for (String order : orderRepository) {
      facility = new Facility();

      FacilityType facilityType = new FacilityType();
      facilityType.setCode(order);
      facilityTypeRepository.save(facilityType);

      GeographicLevel level = new GeographicLevel();
      level.setCode(order);
      level.setLevelNumber(1);
      geographicLevelRepository.save(level);

      GeographicZone geographicZone = new GeographicZone();
      geographicZone.setCode(order);
      geographicZone.setLevel(level);
      geographicZoneRepository.save(geographicZone);

      facility.setType(facilityType);
      facility.setGeographicZone(geographicZone);
      facility.setCode(order);
      facility.setName(order);
      facility.setDescription("Test facility");
      facility.setActive(true);
      facility.setEnabled(true);
      facilityRepository.save(facility);
      testFacilities.add(facility);
    }

    for (String order : orderRepository) {
      program.setCode(order);
      programRepository.save(program);
      testPrograms.add(program);
    }

    Assert.assertEquals(1, userRepository.count());
    user = userRepository.findAll().iterator().next();
    generateTestSet();
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
    for (int i = 0; i < orderRepository.length; i++) {
      Iterable<Order> result = repository.findBySupplyingFacilityAndRequestingFacility(
          testFacilities.get(i), testFacilities.get(i));

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
    for (int i = 0; i < orderRepository.length; i++) {
      Iterable<Order> result = repository.findBySupplyingFacilityAndProgram(
          testFacilities.get(i), testPrograms.get(i));

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
    for ( int i = 0; i < orderRepository.length; i++) {
      Iterable<Order> result = repository.findBySupplyingFacilityAndRequestingFacilityAndProgram(
          testFacilities.get(i), testFacilities.get(i), testPrograms.get(i));

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
