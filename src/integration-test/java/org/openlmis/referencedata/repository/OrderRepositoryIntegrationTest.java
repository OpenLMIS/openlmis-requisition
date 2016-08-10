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
    order.setProgram(generateProgram());
    order.setCreatedBy(generateUser());
    order.setRequestingFacility(generateFacility());
    order.setReceivingFacility(generateFacility());
    order.setSupplyingFacility(generateFacility());
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
  public void searchOrders() {
    List<Order> receivedOrders = repository.searchOrders(
            orders.get(0).getSupplyingFacility(),
            orders.get(0).getRequestingFacility(),
            orders.get(0).getProgram());

    Assert.assertEquals(1, receivedOrders.size());
    for (Order receivedOrder : receivedOrders) {
      Assert.assertEquals(
              receivedOrder.getSupplyingFacility().getId(),
              orders.get(0).getSupplyingFacility().getId());
      Assert.assertEquals(
              receivedOrder.getRequestingFacility().getId(),
              orders.get(0).getRequestingFacility().getId());
      Assert.assertEquals(
              receivedOrder.getProgram().getId(),
              orders.get(0).getProgram().getId());
    }
  }

  private Program generateProgram() {
    Program program = new Program();
    program.setCode("code" +  this.getNextInstanceNumber());
    program.setPeriodsSkippable(false);
    programRepository.save(program);
    return program;
  }

  private Facility generateFacility() {
    Integer instanceNumber = this.getNextInstanceNumber();
    GeographicLevel geographicLevel = generateGeographicLevel();
    GeographicZone geographicZone = generateGeographicZone(geographicLevel);
    FacilityType facilityType = generateFacilityType();
    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("FacilityCode" + instanceNumber);
    facility.setName("FacilityName" + instanceNumber);
    facility.setDescription("FacilityDescription" + instanceNumber);
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);
    return facility;
  }

  private GeographicLevel generateGeographicLevel() {
    GeographicLevel geographicLevel = new GeographicLevel();
    geographicLevel.setCode("GeographicLevel" + this.getNextInstanceNumber());
    geographicLevel.setLevelNumber(1);
    geographicLevelRepository.save(geographicLevel);
    return geographicLevel;
  }

  private GeographicZone generateGeographicZone(GeographicLevel geographicLevel) {
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("GeographicZone" + this.getNextInstanceNumber());
    geographicZone.setLevel(geographicLevel);
    geographicZoneRepository.save(geographicZone);
    return geographicZone;
  }

  private FacilityType generateFacilityType() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode("FacilityType" + this.getNextInstanceNumber());
    facilityTypeRepository.save(facilityType);
    return facilityType;
  }

  private User generateUser() {
    User user = new User();
    Integer instanceNumber = this.getNextInstanceNumber();
    user.setFirstName("Ala" + instanceNumber);
    user.setLastName("ma" + instanceNumber);
    user.setUsername("kota" + instanceNumber);
    user.setPassword("iDobrze" + instanceNumber);
    user.setHomeFacility(generateFacility());
    user.setVerified(true);
    user.setActive(true);
    userRepository.save(user);
    return user;
  }
}
