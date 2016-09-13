package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderNumberConfiguration;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderNumberConfigurationRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.domain.SupplyLine;
import org.openlmis.hierarchyandsupervision.repository.SupervisoryNodeRepository;
import org.openlmis.hierarchyandsupervision.repository.SupplyLineRepository;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.referencedata.domain.ProcessingSchedule;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.openlmis.referencedata.repository.GeographicLevelRepository;
import org.openlmis.referencedata.repository.GeographicZoneRepository;
import org.openlmis.referencedata.repository.ProcessingPeriodRepository;
import org.openlmis.referencedata.repository.ProcessingScheduleRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.service.ReferenceDataService;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.Month;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

@SuppressWarnings("PMD.TooManyMethods")
public class OrderControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/orders";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String ACCESS_TOKEN = "access_token";
  private static final String REQUESTING_FACILITY = "requestingFacility";
  private static final String SUPPLYING_FACILITY = "supplyingFacility";
  private static final String PROGRAM = "program";
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");
  private static final String NUMBER = "10.90";

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
  private OrderNumberConfigurationRepository orderNumberConfigurationRepository;

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private ProcessingPeriodRepository periodRepository;

  @Autowired
  private ProcessingScheduleRepository scheduleRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  @Autowired
  private SupervisoryNodeRepository supervisoryNodeRepository;

  @Autowired
  private SupplyLineRepository supplyLineRepository;

  @Autowired
  private ReferenceDataService referenceDataService;

  private Order firstOrder = new Order();
  private Order secondOrder = new Order();
  private Order thirdOrder = new Order();
  private Requisition requisition;
  private SupplyLine supplyLine;
  private UserDto user;

  @Before
  public void setUp() {
    GeographicLevel geographicLevel = addGeographicLevel("geographicLevelCode", 1);

    FacilityType facilityType = addFacilityType("facilityTypeCode");

    GeographicZone geographicZone = addGeographicZone("geographicZoneCode", geographicLevel);

    Facility facility = addFacility("facilityName", "facilityCode", "facilityDescription",
            facilityType, geographicZone, true, true);

    Program program = addProgram("programCode");

    UserDto[] allUsers = referenceDataService.findAllUsers();
    Assert.assertEquals(1, allUsers.length);
    user = referenceDataService.findOneUser(INITIAL_USER_ID);

    firstOrder = addOrder(null, "orderCode", program, user, facility, facility, facility,
            OrderStatus.ORDERED, new BigDecimal("1.29"));

    ProcessingSchedule schedule1 = addSchedule("Schedule1", "S1");

    ProcessingSchedule schedule2 = addSchedule("Schedule2", "S2");

    Program program1 = addProgram("P1");

    Program program2 = addProgram("P2");

    GeographicLevel geographicLevel1 = addGeographicLevel("GL1", 1);

    GeographicLevel geographicLevel2 = addGeographicLevel("GL2", 1);

    GeographicZone geographicZone1 = addGeographicZone("GZ1", geographicLevel1);

    GeographicZone geographicZone2 = addGeographicZone("GZ2", geographicLevel2);

    FacilityType facilityType1 = addFacilityType("FT1");

    FacilityType facilityType2 = addFacilityType("FT2");

    ProcessingPeriod period1 = addPeriod("P1", schedule1, LocalDate.of(2015, Month.JANUARY, 1),
            LocalDate.of(2015, Month.DECEMBER, 31));

    ProcessingPeriod period2 = addPeriod("P2", schedule2, LocalDate.of(2016, Month.JANUARY, 1),
            LocalDate.of(2016, Month.DECEMBER, 31));

    Facility facility1 = addFacility("facility1", "F1", null, facilityType1,
            geographicZone1, true, false);

    Facility facility2 = addFacility("facility2", "F2", null, facilityType2,
            geographicZone2, true, false);

    Requisition requisition1 = addRequisition(program1, facility1, period1,
            RequisitionStatus.RELEASED, null);

    Requisition requisition2 = addRequisition(program2, facility1, period2,
            RequisitionStatus.RELEASED, null);

    secondOrder = addOrder(requisition1, "O2", program1, user, facility2, facility2,
            facility1, OrderStatus.RECEIVED, new BigDecimal(100));

    thirdOrder = addOrder(requisition2, "O3", program2, user, facility2, facility2,
            facility1, OrderStatus.RECEIVED, new BigDecimal(200));

    ProductCategory productCategory3 = addProductCategory("PCCode1", "PCName1", 1);

    ProductCategory productCategory4 = addProductCategory("PCCode2", "PCName2", 2);

    Product product1 = addProduct("Product1", "P1", "pill", 1, 10, 10, false, true, false, false,
            productCategory3);

    Product product2 = addProduct("Product2", "P2", "pill", 2, 20, 20, true, true, false, false,
            productCategory4);

    addOrderLine(secondOrder, product1, 35L, 50L);

    addOrderLine(secondOrder, product2, 10L, 15L);

    addOrderLine(thirdOrder, product1, 50L, 50L);

    addOrderLine(thirdOrder, product2, 5L, 10L);

    OrderLine orderLine = addOrderLine(firstOrder, product1, 35L, 50L);

    List<OrderLine> orderLines = new ArrayList<>();
    orderLines.add(orderLine);
    firstOrder.setOrderLines(orderLines);

    firstOrder = orderRepository.save(firstOrder);

    geographicLevel = addGeographicLevel("levelCode", 1);

    facilityType = addFacilityType("typeCode");

    geographicZone = addGeographicZone("zoneCode", geographicLevel);

    Facility supplyingFacility = addFacility("supplyingFacilityName", "supplyingFacilityCode",
            "description", facilityType, geographicZone, true, true);

    SupervisoryNode supervisoryNode = addSupervisoryNode(supplyingFacility);

    program = addProgram("progCode");

    ProcessingSchedule schedule = addSchedule("Schedule3", "S3");

    ProcessingPeriod period = addPeriod("P3", schedule, LocalDate.of(2015, Month.JANUARY, 1),
            LocalDate.of(2015, Month.DECEMBER, 31));

    requisition = addRequisition(program, supplyingFacility, period,
            RequisitionStatus.APPROVED, supervisoryNode);

    supplyLine = addSupplyLine(supervisoryNode, program, supplyingFacility);
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

  private Order addOrder(Requisition requisition, String orderCode, Program program, UserDto user,
                         Facility requestingFacility, Facility receivingFacility,
                         Facility supplyingFacility, OrderStatus orderStatus, BigDecimal cost) {
    Order order = new Order();
    order.setRequisition(requisition);
    order.setOrderCode(orderCode);
    order.setQuotedCost(cost);
    order.setStatus(orderStatus);
    order.setProgram(program);
    order.setCreatedById(user.getId());
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
                                     RequisitionStatus requisitionStatus,
                                     SupervisoryNode supervisoryNode) {
    Requisition requisition = new Requisition();
    requisition.setProgram(program);
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(processingPeriod);
    requisition.setStatus(requisitionStatus);
    requisition.setEmergency(false);
    requisition.setSupervisoryNode(supervisoryNode);
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

  private SupervisoryNode addSupervisoryNode(Facility supplyingFacility) {
    SupervisoryNode supervisoryNode = new SupervisoryNode();
    supervisoryNode.setCode("NodeCode");
    supervisoryNode.setName("NodeName");
    supervisoryNode.setFacility(supplyingFacility);
    return supervisoryNodeRepository.save(supervisoryNode);
  }

  private SupplyLine addSupplyLine(SupervisoryNode supervisoryNode, Program program,
                                   Facility supplyingFacility) {
    SupplyLine supplyLine = new SupplyLine();
    supplyLine.setSupervisoryNode(supervisoryNode);
    supplyLine.setProgram(program);
    supplyLine.setSupplyingFacility(supplyingFacility);
    return supplyLineRepository.save(supplyLine);
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

  @Test
  public void shouldNotFinalizeIfWrongOrderStatus() {
    firstOrder.setStatus(OrderStatus.SHIPPED);
    orderRepository.save(firstOrder);

    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .pathParam("id", firstOrder.getId().toString())
            .contentType("application/json")
            .when()
            .put("/api/orders/{id}/finalize")
            .then()
            .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.responseChecks());
  }

  @Test
  public void shouldPrintOrderAsCsv() {
    String csvContent = restAssured.given()
            .queryParam("format", "csv")
            .queryParam(ACCESS_TOKEN, getToken())
            .pathParam("id", secondOrder.getId())
            .when()
            .get("/api/orders/{id}/print")
            .then()
            .statusCode(200)
            .extract().body().asString();

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertTrue(csvContent.startsWith("productName,filledQuantity,orderedQuantity"));
    for (OrderLine o : orderRepository.findOne(secondOrder.getId()).getOrderLines()) {
      assertTrue(csvContent.contains(o.getProduct().getPrimaryName()
              + "," + o.getFilledQuantity()
              + "," + o.getOrderedQuantity()));
    }
  }

  @Test
  public void shouldPrintOrderAsPdf() {
    restAssured.given()
            .queryParam("format", "pdf")
            .queryParam(ACCESS_TOKEN, getToken())
            .pathParam("id", thirdOrder.getId().toString())
            .when()
            .get("/api/orders/{id}/print")
            .then()
            .statusCode(200);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldConvertRequisitionToOrder() {
    orderRepository.deleteAll();
    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration("prefix", true, true, true);
    orderNumberConfigurationRepository.save(orderNumberConfiguration);

    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .body(Collections.singletonList(requisition))
            .when()
            .post("/api/orders/requisitions")
            .then()
            .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertEquals(1, orderRepository.count());
    Order order = orderRepository.findAll().iterator().next();

    assertEquals(user.getId(), order.getCreatedById());
    assertEquals(OrderStatus.ORDERED, order.getStatus());
    assertEquals(order.getRequisition().getId(), requisition.getId());
    assertEquals(order.getReceivingFacility().getId(), requisition.getFacility().getId());
    assertEquals(order.getRequestingFacility().getId(), requisition.getFacility().getId());
    assertEquals(order.getProgram().getId(), requisition.getProgram().getId());
    assertEquals(order.getSupplyingFacility().getId(), supplyLine.getSupplyingFacility().getId());
  }

  @Test
  public void shouldFindBySupplyingFacility() {
    Order[] response = restAssured.given()
            .queryParam(SUPPLYING_FACILITY, firstOrder.getSupplyingFacility().getId())
            .queryParam(ACCESS_TOKEN, getToken())
            .when()
            .get(SEARCH_URL)
            .then()
            .statusCode(200)
            .extract().as(Order[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertEquals(1, response.length);
    for ( Order order : response ) {
      assertEquals(
              order.getSupplyingFacility().getId(),
              firstOrder.getSupplyingFacility().getId());
    }
  }

  @Test
  public void shouldFindBySupplyingFacilityAndRequestingFacility() {
    Order[] response = restAssured.given()
            .queryParam(SUPPLYING_FACILITY, firstOrder.getSupplyingFacility().getId())
            .queryParam(REQUESTING_FACILITY, firstOrder.getRequestingFacility().getId())
            .queryParam(ACCESS_TOKEN, getToken())
            .when()
            .get(SEARCH_URL)
            .then()
            .statusCode(200)
            .extract().as(Order[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertEquals(1, response.length);
    for ( Order order : response ) {
      assertEquals(
              order.getSupplyingFacility().getId(),
              firstOrder.getSupplyingFacility().getId());
      assertEquals(
              order.getRequestingFacility().getId(),
              firstOrder.getRequestingFacility().getId());
    }
  }

  @Test
  public void shouldFindBySupplyingFacilityAndRequestingFacilityAndProgram() {
    Order[] response = restAssured.given()
            .queryParam(SUPPLYING_FACILITY, firstOrder.getSupplyingFacility().getId())
            .queryParam(REQUESTING_FACILITY, firstOrder.getRequestingFacility().getId())
            .queryParam(PROGRAM, firstOrder.getProgram().getId())
            .queryParam(ACCESS_TOKEN, getToken())
            .when()
            .get(SEARCH_URL)
            .then()
            .statusCode(200)
            .extract().as(Order[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertEquals(1, response.length);
    for ( Order order : response ) {
      assertEquals(
              order.getSupplyingFacility().getId(),
              firstOrder.getSupplyingFacility().getId());
      assertEquals(
              order.getRequestingFacility().getId(),
              firstOrder.getRequestingFacility().getId());
      assertEquals(
              order.getProgram().getId(),
              firstOrder.getProgram().getId());
    }
  }

  @Test
  public void shouldDeleteOrder() {

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", firstOrder.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(204);

    assertFalse(orderRepository.exists(firstOrder.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteNonexistentOrder() {

    orderRepository.delete(firstOrder);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", firstOrder.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateOrder() {

    firstOrder.getOrderLines().clear();
    orderRepository.delete(firstOrder);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(firstOrder)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateOrder() {

    firstOrder.setQuotedCost(new BigDecimal(NUMBER));

    Order response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", firstOrder.getId())
          .body(firstOrder)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(Order.class);

    assertEquals(response.getQuotedCost(), new BigDecimal(NUMBER));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewOrderIfDoesNotExist() {

    orderRepository.delete(firstOrder);
    firstOrder.setQuotedCost(new BigDecimal(NUMBER));

    Order response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", ID)
          .body(firstOrder)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(Order.class);

    assertEquals(response.getQuotedCost(), new BigDecimal(NUMBER));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllOrders() {

    Order[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(Order[].class);

    Iterable<Order> orders = Arrays.asList(response);
    assertTrue(orders.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenOrder() {

    Order response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", firstOrder.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(Order.class);

    assertTrue(orderRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonexistentOrder() {

    orderRepository.delete(firstOrder);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", firstOrder.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
