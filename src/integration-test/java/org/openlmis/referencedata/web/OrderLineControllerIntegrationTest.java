package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.SupervisoryNodeRepository;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
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
import org.springframework.http.MediaType;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.Month;
import java.util.Arrays;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

public class OrderLineControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/orderLines";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String ACCESS_TOKEN = "access_token";

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
  private SupervisoryNodeRepository supervisoryNodeRepository;

  private OrderLine orderLine = new OrderLine();
  private User user;

  @Before
  public void setUp() {
    assertEquals(1, userRepository.count());
    user = userRepository.findOne(INITIAL_USER_ID);

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

    Schedule schedule = new Schedule();
    schedule.setCode("scheduleCode");
    schedule.setName("scheduleName");
    scheduleRepository.save(schedule);

    Period period = new Period();
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

    Order order = new Order();
    order.setRequisition(requisition);
    order.setOrderCode("O");
    order.setQuotedCost(new BigDecimal("10.00"));
    order.setStatus(OrderStatus.ORDERED);
    order.setProgram(program);
    order.setCreatedBy(user);
    order.setRequestingFacility(facility);
    order.setReceivingFacility(facility);
    order.setSupplyingFacility(facility);
    orderRepository.save(order);

    orderLine.setOrder(order);
    orderLine.setProduct(product);
    orderLine.setOrderedQuantity(100L);
    orderLine.setFilledQuantity(100L);
    orderLine = orderLineRepository.save(orderLine);
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
}
