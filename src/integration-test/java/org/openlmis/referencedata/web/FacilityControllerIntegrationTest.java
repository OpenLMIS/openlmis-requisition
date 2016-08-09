package org.openlmis.referencedata.web;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.hierarchyandsupervision.service.UserService;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.openlmis.referencedata.repository.GeographicLevelRepository;
import org.openlmis.referencedata.repository.GeographicZoneRepository;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.Month;
import java.util.Iterator;

@SuppressWarnings("PMD.TooManyMethods")
public class FacilityControllerIntegrationTest extends BaseWebIntegrationTest {

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private OrderLineRepository orderLineRepository;

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private UserRepository userRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  @Autowired
  private UserService userService;

  private static final String RESOURCE_URL = BASE_URL + "api/facilities";

  private static final String USERNAME = "testUser";

  private Order order = new Order();
  private User user = new User();
  private Program program = new Program();
  private Period period = new Period();
  private Schedule schedule = new Schedule();

  /** Prepare the test environment. */
  @Before
  public void setUp() {
    schedule = addSchedule("Schedule1", "S1");

    program = addProgram("P1");

    period = addPeriod("P1", schedule, LocalDate.of(2015, Month.JANUARY, 1),
            LocalDate.of(2015, Month.DECEMBER, 31));

    GeographicLevel geographicLevel = addGeographicLevel("GL1", 1);

    GeographicZone geographicZone = addGeographicZone("GZ1", geographicLevel);

    FacilityType facilityType = addFacilityType("FT1");

    Facility facility = addFacility("facility1", "F1", null, facilityType,
            geographicZone, true, false);

    Facility facility2 = addFacility("facility2", "F2", null, facilityType,
            geographicZone, true, false);

    Requisition requisition1 = addRequisition(program, facility, period,
            RequisitionStatus.RELEASED);

    user = userRepository.findOne(INITIAL_USER_ID);
    user.setHomeFacility(facility);
    userRepository.save(user);

    order = addOrder(requisition1, "O2", this.program, this.user, facility2, facility2,
            facility, OrderStatus.RECEIVED, new BigDecimal(100));

    ProductCategory productCategory1 = addProductCategory("PCCode1", "PCName1", 1);

    ProductCategory productCategory2 = addProductCategory("PCCode2", "PCName2", 2);

    Product product1 = addProduct("Product1", "P1", "pill", 1, 10, 10, false, true, false, false,
            productCategory1);

    Product product2 = addProduct("Product2", "P2", "pill", 2, 20, 20, true, true, false, false,
            productCategory2);

    addOrderLine(order, product1, 35L, 50L);

    addOrderLine(order, product2, 10L, 15L);
  }

  /**
   * Cleanup the test environment.
   */
  @After
  public void cleanUp() {
    user.setHomeFacility(null);
    userRepository.save(user);
    orderLineRepository.deleteAll();
    orderRepository.deleteAll();
    requisitionRepository.deleteAll();
    programRepository.deleteAll();
    periodRepository.deleteAll();
    scheduleRepository.deleteAll();
    productRepository.deleteAll();
    productCategoryRepository.deleteAll();
    facilityRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    facilityTypeRepository.deleteAll();

  }

  @Test
  public void testOrderList() throws JsonProcessingException {
    RestTemplate restTemplate = new RestTemplate();

    UriComponentsBuilder builder = UriComponentsBuilder.fromHttpUrl(RESOURCE_URL
            + "/" + user.getHomeFacility().getId() + "/orders")
            .queryParam("program", program.getId())
            .queryParam("access_token",getToken());

    ResponseEntity<Iterable<Order>> orderListResponse = restTemplate.exchange(builder.toUriString(),
            HttpMethod.GET,
            null,
            new ParameterizedTypeReference<Iterable<Order>>() { });

    Iterable<Order> orderList = orderListResponse.getBody();
    Iterator<Order> orderIterator = orderList.iterator();
    Assert.assertTrue(orderIterator.hasNext());
    Order testOrder = orderIterator.next();
    Assert.assertFalse(orderIterator.hasNext());
    Assert.assertEquals(testOrder.getId(), order.getId());
    Assert.assertEquals(testOrder.getRequisition().getId(), order.getRequisition().getId());
    Assert.assertEquals(testOrder.getCreatedBy().getId(), order.getCreatedBy().getId());
    Assert.assertEquals(testOrder.getOrderCode(), order.getOrderCode());
    Assert.assertEquals(testOrder.getOrderLines().size(), 2);
    Assert.assertEquals(testOrder.getCreatedDate(), order.getCreatedDate());
  }

  private Facility addFacility(String facilityName, String facilityCode, String facilityDescription,
                               FacilityType facilityType, GeographicZone geographicZone,
                               boolean isActive, boolean isEnabled) {
    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(facilityCode);
    facility.setName(facilityName);
    facility.setDescription(facilityDescription);
    facility.setActive(isActive);
    facility.setEnabled(isEnabled);
    return facilityRepository.save(facility);
  }

  private Program addProgram(String programCode) {
    Program program = new Program();
    program.setCode(programCode);
    return programRepository.save(program);
  }

  private Order addOrder(Requisition requisition, String orderCode, Program program, User user,
                         Facility requestingFacility, Facility receivingFacility,
                         Facility supplyingFacility, OrderStatus orderStatus, BigDecimal cost) {
    Order order = new Order();
    order.setRequisition(requisition);
    order.setOrderCode(orderCode);
    order.setQuotedCost(cost);
    order.setStatus(orderStatus);
    order.setProgram(program);
    order.setCreatedBy(user);
    order.setRequestingFacility(requestingFacility);
    order.setReceivingFacility(receivingFacility);
    order.setSupplyingFacility(supplyingFacility);
    return orderRepository.save(order);
  }

  private Product addProduct(String productName, String productCode, String dispensingUnit,
                             int dosesPerDispensingUnit, int packSize, int packRoundingThreshold,
                             boolean roundToZero, boolean isActive, boolean isFullSupply,
                             boolean isTraced, ProductCategory productCategory) {
    Product product = new Product();
    product.setPrimaryName(productName);
    product.setCode(productCode);
    product.setDispensingUnit(dispensingUnit);
    product.setDosesPerDispensingUnit(dosesPerDispensingUnit);
    product.setPackSize(packSize);
    product.setPackRoundingThreshold(packRoundingThreshold);
    product.setRoundToZero(roundToZero);
    product.setActive(isActive);
    product.setFullSupply(isFullSupply);
    product.setTracer(isTraced);
    product.setProductCategory(productCategory);
    return productRepository.save(product);
  }

  private Schedule addSchedule(String scheduleName, String scheduleCode) {
    Schedule schedule = new Schedule();
    schedule.setCode(scheduleCode);
    schedule.setName(scheduleName);
    return scheduleRepository.save(schedule);
  }

  private Period addPeriod(String periodName, Schedule processingSchedule,
                           LocalDate startDate, LocalDate endDate) {
    Period period = new Period();
    period.setProcessingSchedule(processingSchedule);
    period.setName(periodName);
    period.setStartDate(startDate);
    period.setEndDate(endDate);
    return periodRepository.save(period);
  }

  private Requisition addRequisition(Program program, Facility facility, Period processingPeriod,
                                     RequisitionStatus requisitionStatus) {
    Requisition requisition = new Requisition();
    requisition.setProgram(program);
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(processingPeriod);
    requisition.setStatus(requisitionStatus);
    return requisitionRepository.save(requisition);
  }

  private OrderLine addOrderLine(Order order, Product product, Long filledQuantity,
                                 Long orderedQuantity) {
    OrderLine orderLine = new OrderLine();
    orderLine.setOrder(order);
    orderLine.setProduct(product);
    orderLine.setOrderedQuantity(orderedQuantity);
    orderLine.setFilledQuantity(filledQuantity);
    return orderLineRepository.save(orderLine);
  }

  private ProductCategory addProductCategory(String code, String name, int displayOrder) {
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCode(code);
    productCategory.setName(name);
    productCategory.setDisplayOrder(displayOrder);
    return productCategoryRepository.save(productCategory);
  }

  private GeographicLevel addGeographicLevel(String code, int level) {
    GeographicLevel geographicLevel = new GeographicLevel();
    geographicLevel.setCode(code);
    geographicLevel.setLevelNumber(level);
    return geographicLevelRepository.save(geographicLevel);
  }

  private GeographicZone addGeographicZone(String code, GeographicLevel geographicLevel) {
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(code);
    geographicZone.setLevel(geographicLevel);
    return geographicZoneRepository.save(geographicZone);
  }

  private FacilityType addFacilityType(String code) {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode(code);
    return facilityTypeRepository.save(facilityType);
  }
}
