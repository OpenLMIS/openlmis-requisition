package org.openlmis.requisition.web;

import guru.nidi.ramltester.junit.RamlMatchers;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
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
import java.util.UUID;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

public class OrderLineControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/orderLines";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String ACCESS_TOKEN = "access_token";
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");

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
  private ReferenceDataService referenceDataService;

  private OrderLine orderLine = new OrderLine();
  private Order order = new Order();
  private UserDto user;

  @Before
  public void setUp() {
    UserDto[] allUsers = referenceDataService.findAllUsers();
    Assert.assertEquals(1, allUsers.length);
    user = referenceDataService.findOneUser(INITIAL_USER_ID);

    ProductCategory productCategory = new ProductCategory();
    productCategory.setCode("PC");
    productCategory.setName("name");
    productCategory.setDisplayOrder(1);
    productCategoryRepository.save(productCategory);

    Product product = new Product();
    product.setPrimaryName("productName");
    product.setCode("productCode");
    product.setDispensingUnit("dispensingUnit");
    product.setDosesPerDispensingUnit(1);
    product.setPackSize(10);
    product.setPackRoundingThreshold(10);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(false);
    product.setTracer(false);
    product.setProductCategory(productCategory);
    productRepository.save(product);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode("F");
    facilityTypeRepository.save(facilityType);

    GeographicLevel geographicLevel = new GeographicLevel();
    geographicLevel.setCode("GL");
    geographicLevel.setLevelNumber(1);
    geographicLevelRepository.save(geographicLevel);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("geographicZoneCode");
    geographicZone.setLevel(geographicLevel);
    geographicZoneRepository.save(geographicZone);

    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("facilityCode");
    facility.setName("facilityName");
    facility.setDescription("facilityDescription");
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    SupervisoryNode supervisoryNode = new SupervisoryNode();
    supervisoryNode.setCode("NodeCode");
    supervisoryNode.setName("NodeName");
    supervisoryNode.setFacility(facility);
    supervisoryNodeRepository.save(supervisoryNode);

    Program program = new Program();
    program.setCode("programCode");
    programRepository.save(program);

    ProcessingSchedule schedule = new ProcessingSchedule();
    schedule.setCode("scheduleCode");
    schedule.setName("scheduleName");
    scheduleRepository.save(schedule);

    ProcessingPeriod period = new ProcessingPeriod();
    period.setProcessingSchedule(schedule);
    period.setName("periodName");
    period.setStartDate(LocalDate.of(2015, Month.JANUARY, 1));
    period.setEndDate(LocalDate.of(2015, Month.DECEMBER, 31));
    periodRepository.save(period);

    Requisition requisition = new Requisition();
    requisition.setProgram(program);
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(period);
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setEmergency(false);
    requisition.setSupervisoryNode(supervisoryNode);
    requisitionRepository.save(requisition);

    orderLine.setOrder(order);
    orderLine.setProduct(product);
    orderLine.setOrderedQuantity(100L);
    orderLine.setFilledQuantity(100L);

    order.setRequisition(requisition);
    order.setOrderCode("O");
    order.setQuotedCost(new BigDecimal("10.00"));
    order.setStatus(OrderStatus.ORDERED);
    order.setProgram(program);
    order.setCreatedById(user.getId());
    order.setRequestingFacility(facility);
    order.setReceivingFacility(facility);
    order.setSupplyingFacility(facility);
    order.setOrderLines(new ArrayList<>());
    order.getOrderLines().add(orderLine);
    orderRepository.save(order);
  }

  @Test
  public void shouldCreateOrder() {

    orderLineRepository.delete(orderLine);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(orderLine)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllOrders() {

    OrderLine[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(OrderLine[].class);

    Iterable<OrderLine> orderLines = Arrays.asList(response);
    assertTrue(orderLines.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateOrderLine() {

    orderLine.setOrderedQuantity(100L);

    OrderLine response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", orderLine.getId())
          .body(orderLine)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(OrderLine.class);

    assertTrue(response.getOrderedQuantity().equals(100L));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewOrderLineIfDoesNotExist() {

    orderLineRepository.delete(orderLine);
    orderLine.setOrderedQuantity(100L);

    OrderLine response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", ID)
          .body(orderLine)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(OrderLine.class);

    assertTrue(response.getOrderedQuantity().equals(100L));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenOrderLine() {

    OrderLine response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", orderLine.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(OrderLine.class);

    assertTrue(orderLineRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonexistentOrderLine() {

    orderLineRepository.delete(orderLine);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", orderLine.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldDeleteOrderLine() {

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", orderLine.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(204);

    assertFalse(orderLineRepository.exists(orderLine.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteNonexistentOrderLine() {

    orderLineRepository.delete(orderLine);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", orderLine.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
