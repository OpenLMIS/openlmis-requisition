package org.openlmis.referencedata.repository;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
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
  };

  private Facility facility = new Facility();
  private Program program = new Program();
  private User user;

  @Before
  public void setUp() {
    facilityRepository.deleteAll();
    programRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    facilityTypeRepository.deleteAll();

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
    }

    for (String order : orderRepository) {
      program.setCode(order);
      programRepository.save(program);
    }

    Assert.assertEquals(1, userRepository.count());
    user = userRepository.findAll().iterator().next();
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

  @After
  public void cleanUp() {
    facilityRepository.deleteAll();
    programRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    repository.deleteAll();
  }
}
