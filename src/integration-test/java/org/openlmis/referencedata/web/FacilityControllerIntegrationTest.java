package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProcessingSchedule;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.openlmis.referencedata.repository.GeographicLevelRepository;
import org.openlmis.referencedata.repository.GeographicZoneRepository;
import org.openlmis.referencedata.repository.ProcessingPeriodRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.repository.ProcessingScheduleRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.Month;
import java.util.Arrays;
import java.util.Iterator;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

@SuppressWarnings("PMD.TooManyMethods")
public class FacilityControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/facilities";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String ACCESS_TOKEN = "access_token";
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");
  private static final String DESCRIPTION = "OpenLMIS";

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
  private ProcessingPeriodRepository periodRepository;

  @Autowired
  private ProcessingScheduleRepository scheduleRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  private Order order = new Order();
  private User user = new User();
  private Program program = new Program();
  private ProcessingPeriod period = new ProcessingPeriod();
  private ProcessingSchedule schedule = new ProcessingSchedule();
  private Facility facility = new Facility();
  private Facility facility2 = new Facility();
  private Requisition requisition = new Requisition();

  @Before
  public void setUp() {
    schedule = addSchedule("Schedule1", "S1");

    program = addProgram("P1");

    period = addPeriod("P1", schedule, LocalDate.of(2015, Month.JANUARY, 1),
            LocalDate.of(2015, Month.DECEMBER, 31));

    GeographicLevel geographicLevel = addGeographicLevel("GL1", 1);

    GeographicZone geographicZone = addGeographicZone("GZ1", geographicLevel);

    FacilityType facilityType = addFacilityType("FT1");

    facility = addFacility("facility1", "F1", null, facilityType,
            geographicZone, true, false);

    user = userRepository.findOne(INITIAL_USER_ID);
    user.setHomeFacility(facility);
    userRepository.save(user);

    facility2 = addFacility("facility2", "F2", null, facilityType,
        geographicZone, true, false);

    requisition = addRequisition(program, facility, period,
        RequisitionStatus.RELEASED);

    order = addOrder(requisition, "O2", this.program, this.user, facility2, facility2,
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

  @Test
  public void shouldFindOrdersFilledByFacility() {
    Order[] response = restAssured.given()
        .queryParam("access_token", getToken())
        .pathParam("id", user.getHomeFacility().getId())
        .queryParam("program", program.getId())
        .queryParam("period", period.getId())
        .queryParam("schedule", schedule.getId())
        .when()
        .get("/api/facilities/{id}/orders")
        .then()
        .statusCode(200)
        .extract().as(Order[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    Iterable<Order> orderList = Arrays.asList(response);
    Iterator<Order> orderIterator = orderList.iterator();
    assertTrue(orderIterator.hasNext());
    Order testOrder = orderIterator.next();
    assertFalse(orderIterator.hasNext());
    assertEquals(testOrder.getId(), order.getId());
    assertEquals(testOrder.getRequisition().getId(), order.getRequisition().getId());
    assertEquals(testOrder.getCreatedBy().getId(), order.getCreatedBy().getId());
    assertEquals(testOrder.getOrderCode(), order.getOrderCode());
    assertEquals(testOrder.getOrderLines().size(), 2);
    assertEquals(testOrder.getCreatedDate(), order.getCreatedDate());
  }

  @Test
  public void shouldDeleteFacility() {

    user.setHomeFacility(null);
    userRepository.save(user);
    order.setSupplyingFacility(facility2);
    orderRepository.save(order);
    requisition.setFacility(facility2);
    requisitionRepository.save(requisition);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", facility.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(204);

    assertFalse(facilityRepository.exists(facility.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateFacility() {

    user.setHomeFacility(null);
    userRepository.save(user);
    order.setSupplyingFacility(facility2);
    orderRepository.save(order);
    requisition.setFacility(facility2);
    requisitionRepository.save(requisition);
    facilityRepository.delete(facility);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(facility)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateFacility() {

    facility.setDescription(DESCRIPTION);

    Facility response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", facility.getId())
          .body(facility)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(Facility.class);

    assertEquals(response.getDescription(), DESCRIPTION);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewFacilityIfDoesNotExists() {

    user.setHomeFacility(null);
    userRepository.save(user);
    order.setSupplyingFacility(facility2);
    orderRepository.save(order);
    requisition.setFacility(facility2);
    requisitionRepository.save(requisition);
    facilityRepository.delete(facility);
    facility.setDescription(DESCRIPTION);

    Facility response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", ID)
          .body(facility)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(Facility.class);

    assertEquals(response.getDescription(), DESCRIPTION);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllFacilities() {

    Facility[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(Facility[].class);

    Iterable<Facility> facilities = Arrays.asList(response);
    assertTrue(facilities.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenFacility() {

    Facility response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", facility.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(Facility.class);

    assertTrue(facilityRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonexistentFacility() {

    user.setHomeFacility(null);
    userRepository.save(user);
    order.setSupplyingFacility(facility2);
    orderRepository.save(order);
    requisition.setFacility(facility2);
    requisitionRepository.save(requisition);
    facilityRepository.delete(facility);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", facility.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
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

  private ProcessingSchedule addSchedule(String scheduleName, String scheduleCode) {
    ProcessingSchedule schedule = new ProcessingSchedule();
    schedule.setCode(scheduleCode);
    schedule.setName(scheduleName);
    return scheduleRepository.save(schedule);
  }

  private ProcessingPeriod addPeriod(String periodName, ProcessingSchedule processingSchedule,
                                     LocalDate startDate, LocalDate endDate) {
    ProcessingPeriod period = new ProcessingPeriod();
    period.setProcessingSchedule(processingSchedule);
    period.setName(periodName);
    period.setStartDate(startDate);
    period.setEndDate(endDate);
    return periodRepository.save(period);
  }

  private Requisition addRequisition(Program program, Facility facility,
                                     ProcessingPeriod processingPeriod,
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
