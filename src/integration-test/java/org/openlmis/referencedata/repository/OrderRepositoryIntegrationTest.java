package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.service.ReferenceDataService;
import org.openlmis.requisition.dto.UserDto;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
public class OrderRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<Order> {

  @Autowired
  private OrderRepository repository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private ReferenceDataService referenceDataService;

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
    order.setCreatedById(UUID.randomUUID());
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
  public void testSearchOrdersByAllParameters() {
    Order order = cloneOrder(orders.get(0));
    List<Order> receivedOrders = repository.searchOrders(
            order.getSupplyingFacility(),
            order.getRequestingFacility(),
            order.getProgram());

    Assert.assertEquals(2, receivedOrders.size());
    for (Order receivedOrder : receivedOrders) {
      Assert.assertEquals(
              order.getSupplyingFacility().getId(),
              receivedOrder.getSupplyingFacility().getId());
      Assert.assertEquals(
              order.getRequestingFacility().getId(),
              receivedOrder.getRequestingFacility().getId());
      Assert.assertEquals(
              order.getProgram().getId(),
              receivedOrder.getProgram().getId());
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
            order.getSupplyingFacility(),
            null,
            order.getProgram());

    Assert.assertEquals(2, receivedOrders.size());
    for (Order receivedOrder : receivedOrders) {
      Assert.assertEquals(
              order.getSupplyingFacility().getId(),
              receivedOrder.getSupplyingFacility().getId());
      Assert.assertEquals(
              order.getProgram().getId(),
              receivedOrder.getProgram().getId());
    }
  }

  private Order cloneOrder(Order order) {
    Order clonedOrder = new Order();
    Integer instanceNumber = this.getNextInstanceNumber();
    clonedOrder.setSupplyingFacility(order.getSupplyingFacility());
    clonedOrder.setRequestingFacility(order.getRequestingFacility());
    clonedOrder.setReceivingFacility(order.getReceivingFacility());
    clonedOrder.setProgram(order.getProgram());
    clonedOrder.setOrderCode(order.getOrderCode() + instanceNumber);
    clonedOrder.setQuotedCost(order.getQuotedCost());
    clonedOrder.setStatus(order.getStatus());
    clonedOrder.setCreatedById(order.getCreatedById());
    clonedOrder.setCreatedDate(order.getCreatedDate());
    repository.save(clonedOrder);
    return  clonedOrder;
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

  private UserDto generateUser() {
    UserDto user = new UserDto();
    Integer instanceNumber = this.getNextInstanceNumber();
    user.setFirstName("Ala" + instanceNumber);
    user.setLastName("ma" + instanceNumber);
    user.setUsername("kota" + instanceNumber);
    user.setEmail(instanceNumber + "@mail.com");
    user.setTimezone("UTC");
    //Fix below after added facility
    user.setHomeFacility(UUID.randomUUID());
    user.setVerified(true);
    user.setActive(true);
    referenceDataService.saveUser(user);
    return user;
  }
}
