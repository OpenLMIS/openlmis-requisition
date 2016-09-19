//package org.openlmis.requisition.web;
//
//import guru.nidi.ramltester.junit.RamlMatchers;
//
//import org.junit.Assert;
//import org.junit.Before;
//import org.junit.Test;
//import org.openlmis.fulfillment.domain.Order;
//import org.openlmis.fulfillment.domain.OrderLine;
//import org.openlmis.fulfillment.domain.OrderStatus;
//import org.openlmis.fulfillment.domain.ProofOfDelivery;
//import org.openlmis.fulfillment.domain.ProofOfDeliveryLine;
//import org.openlmis.fulfillment.repository.OrderLineRepository;
//import org.openlmis.fulfillment.repository.OrderRepository;
//import org.openlmis.fulfillment.repository.ProofOfDeliveryLineRepository;
//import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
//import org.openlmis.requisition.domain.Requisition;
//import org.openlmis.requisition.domain.RequisitionStatus;
//import org.openlmis.requisition.dto.UserDto;
//import org.openlmis.requisition.repository.RequisitionRepository;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.http.MediaType;
//
//import java.math.BigDecimal;
//import java.time.LocalDate;
//import java.time.Month;
//import java.util.ArrayList;
//import java.util.Arrays;
//import java.util.UUID;
//
//import static org.junit.Assert.assertEquals;
//import static org.junit.Assert.assertFalse;
//import static org.junit.Assert.assertThat;
//import static org.junit.Assert.assertTrue;
//
//@SuppressWarnings("PMD.TooManyMethods")
//public class ProofOfDeliveryLineControllerIntegrationTest extends BaseWebIntegrationTest {
//
//  private static final String RESOURCE_URL = "/api/proofOfDeliveryLines";
//  private static final String ID_URL = RESOURCE_URL + "/{id}";
//  private static final String ACCESS_TOKEN = "access_token";
//  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");
//  private static final String NOTES = "OpenLMIS";
//
//  @Autowired
//  private OrderRepository orderRepository;
//
//  @Autowired
//  private OrderLineRepository orderLineRepository;
//
//  @Autowired
//  private ProgramRepository programRepository;
//
//  @Autowired
//  private FacilityRepository facilityRepository;
//
//  @Autowired
//  private ProcessingPeriodRepository periodRepository;
//
//  @Autowired
//  private ProcessingScheduleRepository scheduleRepository;
//
//  @Autowired
//  private ProductRepository productRepository;
//
//  @Autowired
//  private GeographicZoneRepository geographicZoneRepository;
//
//  @Autowired
//  private GeographicLevelRepository geographicLevelRepository;
//
//  @Autowired
//  private FacilityTypeRepository facilityTypeRepository;
//
//  @Autowired
//  private ProductCategoryRepository productCategoryRepository;
//
//  @Autowired
//  private ProofOfDeliveryRepository proofOfDeliveryRepository;
//
//  @Autowired
//  private ProofOfDeliveryLineRepository proofOfDeliveryLineRepository;
//
//  @Autowired
//  private SupervisoryNodeRepository supervisoryNodeRepository;
//
//  @Autowired
//  private RequisitionRepository requisitionRepository;
//
//  @Autowired
//  private ReferenceDataService referenceDataService;
//
//  private UserDto user;
//  private ProofOfDelivery proofOfDelivery = new ProofOfDelivery();
//  private ProofOfDeliveryLine proofOfDeliveryLine = new ProofOfDeliveryLine();
//
//  /**
//   * Prepare the test environment.
//   */
//  @Before
//  public void setUp() {
//    UserDto[] allUsers = referenceDataService.findAllUsers();
//    Assert.assertEquals(1, allUsers.length);
//    user = referenceDataService.findOneUser(INITIAL_USER_ID);
//
//    ProductCategory productCategory = new ProductCategory();
//    productCategory.setCode("PC");
//    productCategory.setName("name");
//    productCategory.setDisplayOrder(1);
//    productCategoryRepository.save(productCategory);
//
//    Product product = new Product();
//    product.setPrimaryName("productName");
//    product.setCode("productCode");
//    product.setDispensingUnit("dispensingUnit");
//    product.setDosesPerDispensingUnit(1);
//    product.setPackSize(10);
//    product.setPackRoundingThreshold(10);
//    product.setRoundToZero(false);
//    product.setActive(true);
//    product.setFullSupply(false);
//    product.setTracer(false);
//    product.setProductCategory(productCategory);
//    productRepository.save(product);
//
//    FacilityType facilityType = new FacilityType();
//    facilityType.setCode("F");
//    facilityTypeRepository.save(facilityType);
//
//    GeographicLevel geographicLevel = new GeographicLevel();
//    geographicLevel.setCode("GL");
//    geographicLevel.setLevelNumber(1);
//    geographicLevelRepository.save(geographicLevel);
//
//    GeographicZone geographicZone = new GeographicZone();
//    geographicZone.setCode("geographicZoneCode");
//    geographicZone.setLevel(geographicLevel);
//    geographicZoneRepository.save(geographicZone);
//
//    Facility facility = new Facility();
//    facility.setType(facilityType);
//    facility.setGeographicZone(geographicZone);
//    facility.setCode("facilityCode");
//    facility.setName("facilityName");
//    facility.setDescription("facilityDescription");
//    facility.setActive(true);
//    facility.setEnabled(true);
//    facilityRepository.save(facility);
//
//    SupervisoryNode supervisoryNode = new SupervisoryNode();
//    supervisoryNode.setCode("NodeCode");
//    supervisoryNode.setName("NodeName");
//    supervisoryNode.setFacility(facility);
//    supervisoryNodeRepository.save(supervisoryNode);
//
//    Program program = new Program();
//    program.setCode("programCode");
//    programRepository.save(program);
//
//    ProcessingSchedule schedule = new ProcessingSchedule();
//    schedule.setCode("scheduleCode");
//    schedule.setName("scheduleName");
//    scheduleRepository.save(schedule);
//
//    ProcessingPeriod period = new ProcessingPeriod();
//    period.setProcessingSchedule(schedule);
//    period.setName("periodName");
//    period.setStartDate(LocalDate.of(2015, Month.JANUARY, 1));
//    period.setEndDate(LocalDate.of(2015, Month.DECEMBER, 31));
//    periodRepository.save(period);
//
//    Requisition requisition = new Requisition();
//    requisition.setProgram(program);
//    requisition.setFacility(facility);
//    requisition.setProcessingPeriod(period);
//    requisition.setStatus(RequisitionStatus.INITIATED);
//    requisition.setEmergency(false);
//    requisition.setSupervisoryNode(supervisoryNode);
//    requisitionRepository.save(requisition);
//
//    Order order = new Order();
//    order.setRequisition(requisition);
//    order.setOrderCode("O");
//    order.setQuotedCost(new BigDecimal("10.00"));
//    order.setStatus(OrderStatus.ORDERED);
//    order.setProgram(program);
//    order.setCreatedById(user.getId());
//    order.setRequestingFacility(facility);
//    order.setReceivingFacility(facility);
//    order.setSupplyingFacility(facility);
//    orderRepository.save(order);
//
//    OrderLine orderLine = new OrderLine();
//    orderLine.setOrder(order);
//    orderLine.setProduct(product);
//    orderLine.setOrderedQuantity(100L);
//    orderLine.setFilledQuantity(100L);
//    orderLineRepository.save(orderLine);
//
//    proofOfDeliveryLine.setOrderLine(orderLine);
//    proofOfDeliveryLine.setProofOfDelivery(proofOfDelivery);
//    proofOfDeliveryLine.setQuantityShipped(100L);
//    proofOfDeliveryLine.setQuantityReturned(100L);
//    proofOfDeliveryLine.setQuantityReceived(100L);
//    proofOfDeliveryLine.setPackToShip(100L);
//    proofOfDeliveryLine.setReplacedProductCode("replaced product code");
//    proofOfDeliveryLine.setNotes("Notes");
//
//    proofOfDelivery.setOrder(order);
//    proofOfDelivery.setTotalShippedPacks(100);
//    proofOfDelivery.setTotalReceivedPacks(100);
//    proofOfDelivery.setTotalReturnedPacks(10);
//    proofOfDelivery.setDeliveredBy("delivered by");
//    proofOfDelivery.setReceivedBy("received by");
//    proofOfDelivery.setReceivedDate(LocalDate.now());
//    proofOfDelivery.setProofOfDeliveryLineItems(new ArrayList<>());
//    proofOfDelivery.getProofOfDeliveryLineItems().add(proofOfDeliveryLine);
//    proofOfDeliveryRepository.save(proofOfDelivery);
//  }
//
//  @Test
//  public void shouldDeleteProofOfDeliveryLine() {
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", proofOfDeliveryLine.getId())
//          .when()
//          .delete(ID_URL)
//          .then()
//          .statusCode(204);
//
//    assertFalse(proofOfDeliveryLineRepository.exists(proofOfDeliveryLine.getId()));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotDeleteNonexistentProofOfDeliveryLine() {
//
//    proofOfDeliveryLineRepository.delete(proofOfDeliveryLine);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", proofOfDeliveryLine.getId())
//          .when()
//          .delete(ID_URL)
//          .then()
//          .statusCode(404);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldGetAllProofOfDeliveryLines() {
//
//    ProofOfDeliveryLine[] response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .when()
//          .get(RESOURCE_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(ProofOfDeliveryLine[].class);
//
//    Iterable<ProofOfDeliveryLine> proofOfDeliveryLines = Arrays.asList(response);
//    assertTrue(proofOfDeliveryLines.iterator().hasNext());
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldGetChosenProofOfDeliveryLine() {
//
//    ProofOfDeliveryLine response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", proofOfDeliveryLine.getId())
//          .when()
//          .get(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(ProofOfDeliveryLine.class);
//
//    assertTrue(proofOfDeliveryLineRepository.exists(response.getId()));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotGetNonexistentProofOfDeliveryLine() {
//
//    proofOfDeliveryLineRepository.delete(proofOfDeliveryLine);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", proofOfDeliveryLine.getId())
//          .when()
//          .get(ID_URL)
//          .then()
//          .statusCode(404);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldCreateProofOfDeliveryLine() {
//
//    proofOfDeliveryLineRepository.delete(proofOfDeliveryLine);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .body(proofOfDeliveryLine)
//          .when()
//          .post(RESOURCE_URL)
//          .then()
//          .statusCode(201);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldUpdateProofOfDeliveryLine() {
//
//    proofOfDeliveryLine.setNotes(NOTES);
//
//    ProofOfDeliveryLine response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", proofOfDeliveryLine.getId())
//          .body(proofOfDeliveryLine)
//          .when()
//          .put(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(ProofOfDeliveryLine.class);
//
//    assertEquals(response.getNotes(), NOTES);
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldCreateNewProofOfDeliveryLineIfDoesNotExist() {
//
//    proofOfDeliveryLineRepository.delete(proofOfDeliveryLine);
//    proofOfDeliveryLine.setNotes(NOTES);
//
//    ProofOfDeliveryLine response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", ID)
//          .body(proofOfDeliveryLine)
//          .when()
//          .put(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(ProofOfDeliveryLine.class);
//
//    assertEquals(response.getNotes(), NOTES);
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//}
