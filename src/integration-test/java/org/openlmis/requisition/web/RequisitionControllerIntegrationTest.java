//package org.openlmis.requisition.web;
//
//import static java.lang.String.valueOf;
//import static org.junit.Assert.assertEquals;
//import static org.junit.Assert.assertFalse;
//import static org.junit.Assert.assertNotNull;
//import static org.junit.Assert.assertThat;
//import static org.junit.Assert.assertTrue;
//
//import guru.nidi.ramltester.junit.RamlMatchers;
//import org.junit.Assert;
//import org.junit.Before;
//import org.junit.Test;
//import org.openlmis.requisition.domain.Comment;
//import org.openlmis.requisition.domain.Requisition;
//import org.openlmis.requisition.domain.RequisitionLine;
//import org.openlmis.requisition.domain.RequisitionStatus;
//import org.openlmis.requisition.dto.UserDto;
//import org.openlmis.requisition.repository.CommentRepository;
//import org.openlmis.requisition.repository.RequisitionLineRepository;
//import org.openlmis.requisition.repository.RequisitionRepository;
//import org.openlmis.settings.domain.ConfigurationSetting;
//import org.openlmis.settings.repository.ConfigurationSettingRepository;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.http.MediaType;
//
//import java.time.LocalDate;
//import java.time.LocalDateTime;
//import java.util.ArrayList;
//import java.util.Arrays;
//import java.util.Iterator;
//import java.util.List;
//import java.util.UUID;
//import java.util.concurrent.atomic.AtomicInteger;
//
//@SuppressWarnings("PMD.TooManyMethods")
//public class RequisitionControllerIntegrationTest extends BaseWebIntegrationTest {
//
//  private static final String ACCESS_TOKEN = "access_token";
// private static final String REQUISITION_REPOSITORY_NAME = "RequisitionRepositoryIntegrationTest";
//  private static final String EXPECTED_MESSAGE_FIRST_PART = "{\n  \"requisitionLines\" : ";
//  private static final String RESOURCE_URL = "/api/requisitions";
//  private static final String INSERT_COMMENT = RESOURCE_URL + "/{id}/comments";
//  private static final String INITIATE_URL = RESOURCE_URL + "/initiate";
//  private static final String APPROVE_REQUISITION = RESOURCE_URL + "/{id}/approve";
//  private static final String SKIP_URL = RESOURCE_URL + "/{id}/skip";
//  private static final String REJECT_URL = RESOURCE_URL + "/{id}/reject";
//  private static final String SUBMIT_URL = RESOURCE_URL + "/{id}/submit";
//  private static final String SUBMITTED_URL = RESOURCE_URL + "/submitted";
//  private static final String AUTHORIZATION_URL = RESOURCE_URL + "/{id}/authorize";
//  private static final String ID_COMMENT_URL = RESOURCE_URL + "/comments/{id}";
//  private static final String ID_URL = RESOURCE_URL + "/{id}";
//  private static final String SEARCH_URL = RESOURCE_URL + "/search";
//  private static final String REQ_FOR_APPROVAL_URL = RESOURCE_URL + "/requisitions-for-approval";
//  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");
//  private static final String COMMENT_TEXT = "OpenLMIS";
//  private static final String COMMENT = "Comment";
// private static final String APPROVED_REQUISITIONS_SEARCH_URL = RESOURCE_URL + "/approved/search";
//
//  @Autowired
//  private ProductRepository productRepository;
//
//  @Autowired
//  private RequisitionLineRepository requisitionLineRepository;
//
//  @Autowired
//  private ProgramRepository programRepository;
//
//  @Autowired
//  private ProcessingPeriodRepository periodRepository;
//
//  @Autowired
//  private ProcessingScheduleRepository scheduleRepository;
//
//  @Autowired
//  private FacilityRepository facilityRepository;
//
//  @Autowired
//  private RequisitionRepository requisitionRepository;
//
//  @Autowired
//  private GeographicLevelRepository geographicLevelRepository;
//
//  @Autowired
//  private GeographicZoneRepository geographicZoneRepository;
//
//  @Autowired
//  private FacilityTypeRepository facilityTypeRepository;
//
//  @Autowired
//  private ProductCategoryRepository productCategoryRepository;
//
//  @Autowired
//  private CommentRepository commentRepository;
//
//  @Autowired
//  private ConfigurationSettingRepository configurationSettingRepository;
//
//  @Autowired
//  private SupervisoryNodeRepository supervisoryNodeRepository;
//
//  @Autowired
//  private ReferenceDataService referenceDataService;
//
//  private RequisitionLine requisitionLine = new RequisitionLine();
//  private Requisition requisition = new Requisition();
//  private ProcessingPeriod period = new ProcessingPeriod();
//  private Product product = new Product();
//  private Program program = new Program();
//  private Facility facility = new Facility();
//  private SupervisoryNode supervisoryNode = new SupervisoryNode();
//  private UserDto user;
//  private LocalDateTime localDateTime = LocalDateTime.now();
//
//  private AtomicInteger instanceNumber = new AtomicInteger(0);
//
//  private int getNextInstanceNumber() {
//    return this.instanceNumber.incrementAndGet();
//  }
//
//  @Before
//  public void setUp() {
//    ProductCategory productCategory1 = new ProductCategory();
//    productCategory1.setCode("PC1");
//    productCategory1.setName("PC1 name");
//    productCategory1.setDisplayOrder(1);
//    productCategoryRepository.save(productCategory1);
//
//    product.setCode(REQUISITION_REPOSITORY_NAME);
//    product.setPrimaryName(REQUISITION_REPOSITORY_NAME);
//    product.setDispensingUnit(REQUISITION_REPOSITORY_NAME);
//    product.setDosesPerDispensingUnit(10);
//    product.setPackSize(1);
//    product.setPackRoundingThreshold(0);
//    product.setRoundToZero(false);
//    product.setActive(true);
//    product.setFullSupply(true);
//    product.setTracer(false);
//    product.setProductCategory(productCategory1);
//    productRepository.save(product);
//
//    program.setCode(REQUISITION_REPOSITORY_NAME);
//    program.setPeriodsSkippable(true);
//    programRepository.save(program);
//
//    FacilityType facilityType = new FacilityType();
//    facilityType.setCode(REQUISITION_REPOSITORY_NAME);
//    facilityTypeRepository.save(facilityType);
//
//    GeographicLevel level = new GeographicLevel();
//    level.setCode(REQUISITION_REPOSITORY_NAME);
//    level.setLevelNumber(1);
//    geographicLevelRepository.save(level);
//
//    GeographicZone geographicZone = new GeographicZone();
//    geographicZone.setCode(REQUISITION_REPOSITORY_NAME);
//    geographicZone.setLevel(level);
//    geographicZoneRepository.save(geographicZone);
//
//    facility.setType(facilityType);
//    facility.setGeographicZone(geographicZone);
//    facility.setCode(REQUISITION_REPOSITORY_NAME);
//    facility.setActive(true);
//    facility.setEnabled(true);
//    facilityRepository.save(facility);
//
//    ProcessingSchedule schedule = new ProcessingSchedule();
//    schedule.setCode(REQUISITION_REPOSITORY_NAME);
//    schedule.setName(REQUISITION_REPOSITORY_NAME);
//    scheduleRepository.save(schedule);
//
//    period.setName(REQUISITION_REPOSITORY_NAME);
//    period.setProcessingSchedule(schedule);
//    period.setDescription(REQUISITION_REPOSITORY_NAME);
//    period.setStartDate(LocalDate.of(2016, 1, 1));
//    period.setEndDate(LocalDate.of(2016, 2, 1));
//    periodRepository.save(period);
//
//    supervisoryNode.setName("name");
//    supervisoryNode.setCode("code");
//    supervisoryNode.setDescription("description");
//    supervisoryNode.setFacility(facility);
//    supervisoryNodeRepository.save(supervisoryNode);
//
//    configureRequisition(requisition);
//
//    requisitionLine.setProduct(product);
//    requisitionLine.setRequestedQuantity(1);
//    requisitionLine.setStockOnHand(1);
//    requisitionLine.setTotalConsumedQuantity(1);
//    requisitionLine.setBeginningBalance(1);
//    requisitionLine.setTotalReceivedQuantity(1);
//    requisitionLine.setTotalLossesAndAdjustments(1);
//    requisitionLineRepository.save(requisitionLine);
//
//    List<RequisitionLine> requisitionLines = new ArrayList<>();
//    requisitionLines.add(requisitionLine);
//
//    user = referenceDataService.findOneUser(INITIAL_USER_ID);
//
//    requisition.setRequisitionLines(requisitionLines);
//    requisition = requisitionRepository.save(requisition);
//    requisitionRepository.save(requisition);
//  }
//
//  @Test
//  public void shouldFindRequisitions() {
//    Requisition[] response = restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .queryParam("program", program.getId())
//            .queryParam("processingPeriod", period.getId())
//            .queryParam("facility", facility.getId())
//            .queryParam("supervisoryNode", supervisoryNode.getId())
//            .queryParam("requisitionStatus", RequisitionStatus.INITIATED)
//            .queryParam("createdDateFrom", localDateTime.minusDays(2).toString())
//            .queryParam("createdDateTo", localDateTime.plusDays(2).toString())
//            .when()
//            .get(SEARCH_URL)
//            .then()
//            .statusCode(200)
//            .extract().as(Requisition[].class);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//    assertEquals(1, response.length);
//    for ( Requisition receivedRequisition : response ) {
//      assertEquals(
//              receivedRequisition.getProgram().getId(),
//              program.getId());
//      assertEquals(
//              receivedRequisition.getProcessingPeriod().getId(),
//              period.getId());
//      assertEquals(
//              receivedRequisition.getFacility().getId(),
//              facility.getId());
//      assertEquals(
//              receivedRequisition.getSupervisoryNode().getId(),
//              supervisoryNode.getId());
//      assertEquals(
//              receivedRequisition.getStatus(),
//              RequisitionStatus.INITIATED);
//      assertTrue(
//              receivedRequisition.getCreatedDate().isBefore(localDateTime.plusDays(2)));
//      assertTrue(
//              receivedRequisition.getCreatedDate().isAfter(localDateTime.minusDays(2)));
//    }
//  }
//
//  @Test
//  public void shouldSubmitCorrectRequisition() {
//
//    Requisition response = restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .pathParam("id", requisition.getId())
//            .body(requisition)
//            .when()
//            .put(SUBMIT_URL)
//            .then()
//            .statusCode(200)
//            .extract().as(Requisition.class);
//
//    assertNotNull(response.getId());
//    assertEquals(requisition.getId(), response.getId());
//    assertEquals(RequisitionStatus.SUBMITTED, response.getStatus());
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotSubmitRequisitionWithNullRequisitionLines() {
//
//    requisition = configureRequisition(new Requisition());
//
//    String response = restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .pathParam("id", requisition.getId())
//            .body(requisition)
//            .when()
//            .put(SUBMIT_URL)
//            .then()
//            .statusCode(400)
//            .extract().asString();
//
//    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
//            + "\"A requisitionLines must be entered prior to submission of a requisition.\"\n}";
//
//    assertTrue(response.contains(expectedExceptionMessage));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotSubmitRequisitionWithNullQuantityInRequisitionLine() {
//
//    RequisitionLine requisitionLine = new RequisitionLine();
//    requisitionLine.setProduct(product);
//    requisitionLine.setStockOnHand(1);
//    requisitionLine.setTotalConsumedQuantity(1);
//    requisitionLine.setBeginningBalance(1);
//    requisitionLine.setTotalReceivedQuantity(1);
//    requisitionLine.setTotalLossesAndAdjustments(1);
//    requisitionLineRepository.save(requisitionLine);
//
//    List<RequisitionLine> requisitionLines = new ArrayList<>();
//    requisitionLines.add(requisitionLine);
//
//    requisition.setRequisitionLines(requisitionLines);
//    requisition = requisitionRepository.save(requisition);
//
//    String response = restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .pathParam("id", requisition.getId())
//            .body(requisition)
//            .when()
//            .put(SUBMIT_URL)
//            .then()
//            .statusCode(400)
//            .extract().asString();
//
//    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
//            + "\"A quantity must be entered prior to submission of a requisition.\"\n}";
//
//    assertTrue(response.contains(expectedExceptionMessage));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotSubmitRequisitionWithNullBeginningBalanceInRequisitionLine() {
//
//    RequisitionLine requisitionLine = new RequisitionLine();
//    requisitionLine.setRequestedQuantity(1);
//    requisitionLine.setProduct(product);
//    requisitionLine.setStockOnHand(1);
//    requisitionLine.setTotalConsumedQuantity(1);
//    requisitionLine.setTotalReceivedQuantity(1);
//    requisitionLine.setTotalLossesAndAdjustments(1);
//    requisitionLineRepository.save(requisitionLine);
//
//    List<RequisitionLine> requisitionLines = new ArrayList<>();
//    requisitionLines.add(requisitionLine);
//
//    requisition.setRequisitionLines(requisitionLines);
//    requisition = requisitionRepository.save(requisition);
//
//    String response = restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .pathParam("id", requisition.getId())
//            .body(requisition)
//            .when()
//            .put(SUBMIT_URL)
//            .then()
//            .statusCode(400)
//            .extract().asString();
//
//    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
//            + "\"A beginning balance must be entered prior to submission of a requisition.\"\n}";
//
//    assertTrue(response.contains(expectedExceptionMessage));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotSubmitRequisitionWithNegativeBeginningBalanceInRequisitionLine() {
//
//    RequisitionLine requisitionLine = new RequisitionLine();
//    requisitionLine.setRequestedQuantity(1);
//    requisitionLine.setBeginningBalance(-1);
//    requisitionLine.setProduct(product);
//    requisitionLine.setStockOnHand(1);
//    requisitionLine.setTotalConsumedQuantity(1);
//    requisitionLine.setTotalReceivedQuantity(1);
//    requisitionLine.setTotalLossesAndAdjustments(1);
//    requisitionLineRepository.save(requisitionLine);
//
//    List<RequisitionLine> requisitionLines = new ArrayList<>();
//    requisitionLines.add(requisitionLine);
//
//    requisition.setRequisitionLines(requisitionLines);
//    requisition = requisitionRepository.save(requisition);
//
//    String response = restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .pathParam("id", requisition.getId())
//            .body(requisition)
//            .when()
//            .put(SUBMIT_URL)
//            .then()
//            .statusCode(400)
//            .extract().asString();
//
//    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
//            + "\"A beginning balance must be a non-negative value.\"\n}";
//
//    assertTrue(response.contains(expectedExceptionMessage));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotSubmitRequisitionWithNullTotalReceivedQuantityInRequisitionLine() {
//
//    RequisitionLine requisitionLine = new RequisitionLine();
//    requisitionLine.setRequestedQuantity(1);
//    requisitionLine.setProduct(product);
//    requisitionLine.setStockOnHand(1);
//    requisitionLine.setTotalConsumedQuantity(1);
//    requisitionLine.setTotalLossesAndAdjustments(1);
//    requisitionLineRepository.save(requisitionLine);
//
//    List<RequisitionLine> requisitionLines = new ArrayList<>();
//    requisitionLines.add(requisitionLine);
//
//    requisition.setRequisitionLines(requisitionLines);
//    requisition = requisitionRepository.save(requisition);
//
//    String response = restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .pathParam("id", requisition.getId())
//            .body(requisition)
//            .when()
//            .put(SUBMIT_URL)
//            .then()
//            .statusCode(400)
//            .extract().asString();
//
//    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
//            + "\"A total received quantity"
//            + " must be entered prior to submission of a requisition.\"\n}";
//
//    assertTrue(response.contains(expectedExceptionMessage));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotSubmitRequisitionWithNegativeTotalReceivedQuantityInRequisitionLine() {
//
//    RequisitionLine requisitionLine = new RequisitionLine();
//    requisitionLine.setRequestedQuantity(1);
//    requisitionLine.setBeginningBalance(1);
//    requisitionLine.setProduct(product);
//    requisitionLine.setStockOnHand(1);
//    requisitionLine.setTotalConsumedQuantity(1);
//    requisitionLine.setTotalReceivedQuantity(-1);
//    requisitionLine.setTotalLossesAndAdjustments(1);
//    requisitionLineRepository.save(requisitionLine);
//
//    List<RequisitionLine> requisitionLines = new ArrayList<>();
//    requisitionLines.add(requisitionLine);
//
//    requisition.setRequisitionLines(requisitionLines);
//    requisition = requisitionRepository.save(requisition);
//
//    String response = restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .pathParam("id", requisition.getId())
//            .body(requisition)
//            .when()
//            .put(SUBMIT_URL)
//            .then()
//            .statusCode(400)
//            .extract().asString();
//
//    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
//            + "\"A total received quantity must be a non-negative value.\"\n}";
//
//    assertTrue(response.contains(expectedExceptionMessage));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotSubmitRequisitionWithNullStockHandInRequisitionLine() {
//
//    RequisitionLine requisitionLine = new RequisitionLine();
//    requisitionLine.setRequestedQuantity(1);
//    requisitionLine.setBeginningBalance(1);
//    requisitionLine.setProduct(product);
//    requisitionLine.setTotalConsumedQuantity(1);
//    requisitionLine.setTotalReceivedQuantity(1);
//    requisitionLine.setTotalLossesAndAdjustments(1);
//    requisitionLineRepository.save(requisitionLine);
//
//    List<RequisitionLine> requisitionLines = new ArrayList<>();
//    requisitionLines.add(requisitionLine);
//
//    requisition.setRequisitionLines(requisitionLines);
//    requisition = requisitionRepository.save(requisition);
//
//    String response = restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .pathParam("id", requisition.getId())
//            .body(requisition)
//            .when()
//            .put(SUBMIT_URL)
//            .then()
//            .statusCode(400)
//            .extract().asString();
//
//    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
//           + "\"A total stock on hand must be entered prior to submission of a requisition.\"\n}";
//
//    assertTrue(response.contains(expectedExceptionMessage));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotSubmitRequisitionWithNullConsumedQuantityInRequisitionLine() {
//
//    RequisitionLine requisitionLine = new RequisitionLine();
//    requisitionLine.setRequestedQuantity(1);
//    requisitionLine.setBeginningBalance(1);
//    requisitionLine.setProduct(product);
//    requisitionLine.setTotalReceivedQuantity(1);
//    requisitionLine.setTotalLossesAndAdjustments(1);
//    requisitionLine.setStockOnHand(1);
//    requisitionLineRepository.save(requisitionLine);
//
//    List<RequisitionLine> requisitionLines = new ArrayList<>();
//    requisitionLines.add(requisitionLine);
//
//    requisition.setRequisitionLines(requisitionLines);
//    requisition = requisitionRepository.save(requisition);
//
//    String response = restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .pathParam("id", requisition.getId())
//            .body(requisition)
//            .when()
//            .put(SUBMIT_URL)
//            .then()
//            .statusCode(400)
//            .extract().asString();
//
//    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
//            + "\"A total consumed quantity"
//            + " must be entered prior to submission of a requisition.\"\n}";
//
//    assertTrue(response.contains(expectedExceptionMessage));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotSubmitRequisitionWithNullAttributesInRequisitionLine() {
//
//    RequisitionLine requisitionLine = new RequisitionLine();
//    requisitionLine.setProduct(product);
//    requisitionLine.setStockOnHand(null);
//    requisitionLine.setTotalConsumedQuantity(null);
//    requisitionLine.setBeginningBalance(null);
//    requisitionLine.setTotalReceivedQuantity(null);
//    requisitionLine.setTotalLossesAndAdjustments(null);
//    requisitionLineRepository.save(requisitionLine);
//
//    List<RequisitionLine> requisitionLines = new ArrayList<>();
//    requisitionLines.add(requisitionLine);
//
//    requisition.setRequisitionLines(requisitionLines);
//    requisition = requisitionRepository.save(requisition);
//
//    String response = restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .pathParam("id", requisition.getId())
//            .body(requisition)
//            .when()
//            .put(SUBMIT_URL)
//            .then()
//            .statusCode(400)
//            .extract().asString();
//
//    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
//            + "\"A total losses and adjustments must be entered prior "
//            + "to submission of a requisition.\"\n}";
//
//    assertTrue(response.contains(expectedExceptionMessage));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldSkipRequisition() {
//    restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .pathParam("id", requisition.getId())
//            .when()
//            .put(SKIP_URL)
//            .then()
//            .statusCode(200);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldRejectRequisition() {
//
//    requisition.setStatus(RequisitionStatus.AUTHORIZED);
//    requisitionRepository.save(requisition);
//
//    restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .pathParam("id", requisition.getId())
//            .when()
//            .put(REJECT_URL)
//            .then()
//            .statusCode(200);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotRejectWithWrongStatus() {
//
//    restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .pathParam("id", requisition.getId())
//            .when()
//            .put(REJECT_URL)
//            .then()
//            .statusCode(400);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldDeleteRequisition() {
//
//    requisition.setStatus(RequisitionStatus.INITIATED);
//    requisitionRepository.save(requisition);
//
//    restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .pathParam("id", requisition.getId())
//            .when()
//            .delete(ID_URL)
//            .then()
//            .statusCode(204);
//
//    assertFalse(requisitionRepository.exists(requisition.getId()));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotDeleteNonexistentRequisition() {
//
//    requisitionRepository.delete(requisition);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisition.getId())
//          .when()
//          .delete(ID_URL)
//          .then()
//          .statusCode(400);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotDeleteRequisitionWithWrongStatus() {
//
//    requisition.setStatus(RequisitionStatus.SUBMITTED);
//    requisitionRepository.save(requisition);
//
//    restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .pathParam("id", requisition.getId())
//            .when()
//            .delete(ID_URL)
//            .then()
//            .statusCode(400);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldGetCommentsForRequisition() {
//    createComment(user, requisition, "First comment");
//    createComment(user, requisition, "Second comment");
//
//    requisition.setStatus(RequisitionStatus.AUTHORIZED);
//    requisitionRepository.save(requisition);
//
//    Comment[] response = restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .pathParam("id", requisition.getId())
//            .when()
//            .get(INSERT_COMMENT)
//            .then()
//            .statusCode(200)
//            .extract().as(Comment[].class);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//    List<Comment> commentList = Arrays.asList(response);
//    assertEquals("First comment", commentList.get(0).getBody());
//    assertEquals("Second comment", commentList.get(1).getBody());
//  }
//
//  @Test
//  public void shouldGetRequisitionsForApprovalForSpecificUser() {
//    requisition.setSupervisoryNode(supervisoryNode);
//    requisition.setStatus(RequisitionStatus.AUTHORIZED);
//    requisitionRepository.save(requisition);
//
//    user.setSupervisedNode(supervisoryNode.getId());
//    referenceDataService.saveUser(user);
//
//    Requisition[] response = restAssured.given()
//        .queryParam(ACCESS_TOKEN, getToken())
//        .contentType(MediaType.APPLICATION_JSON_VALUE)
//        .when()
//        .get(REQ_FOR_APPROVAL_URL)
//        .then()
//        .statusCode(200)
//        .extract().as(Requisition[].class);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//    List<Requisition> responseList = Arrays.asList(response);
//    List<Requisition> expectedRequisitionList = new ArrayList<>();
//    expectedRequisitionList.add(requisition);
//
//    for (int i = 0; i < responseList.size(); i++) {
//      assertEquals(expectedRequisitionList.get(i).getId(), responseList.get(i).getId());
//    }
//    user.setSupervisedNode(null);
//    referenceDataService.saveUser(user);
//  }
//
//  @Test
//  public void shouldInsertComment() {
//
//    requisition.setStatus(RequisitionStatus.AUTHORIZED);
//    requisitionRepository.save(requisition);
//
//    createComment(user, requisition, "Previous comment");
//    Comment userPostComment = new Comment();
//    userPostComment.setBody("User comment");
//
//    Comment[] response = restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .body(userPostComment)
//            .pathParam("id", requisition.getId())
//            .when()
//            .post(INSERT_COMMENT)
//            .then()
//            .statusCode(200)
//            .extract().as(Comment[].class);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//    List<Comment> commentList = Arrays.asList(response);
//    assertEquals("Previous comment", commentList.get(0).getBody());
//    assertEquals("User comment", commentList.get(1).getBody());
//  }
//
//  @Test
//  public void shouldGetChosenComment() {
//
//    Comment comment = createComment(user, requisition, COMMENT);
//
//    Comment response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", comment.getId())
//          .when()
//          .get(ID_COMMENT_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(Comment.class);
//
//    assertTrue(commentRepository.exists(response.getId()));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotGetNonexistentComment() {
//
//    Comment comment = createComment(user, requisition, COMMENT);
//    commentRepository.delete(comment);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", comment.getId())
//          .when()
//          .get(ID_COMMENT_URL)
//          .then()
//          .statusCode(404);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldCreateNewCommentIfDoesNotExist() {
//
//    requisition.setStatus(RequisitionStatus.AUTHORIZED);
//    requisitionRepository.save(requisition);
//
//    Comment comment = new Comment();
//    comment.setBody(COMMENT_TEXT);
//    comment.setAuthorId(user.getId());
//    comment.setRequisition(requisition);
//
//    Comment response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", ID)
//          .body(comment)
//          .when()
//          .put(ID_COMMENT_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(Comment.class);
//
//    assertEquals(response.getBody(), COMMENT_TEXT);
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldUpdateComment() {
//
//    requisition.setStatus(RequisitionStatus.AUTHORIZED);
//    requisitionRepository.save(requisition);
//
//    Comment comment = createComment(user, requisition, COMMENT);
//    comment.setBody(COMMENT_TEXT);
//
//    Comment response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", comment.getId())
//          .body(comment)
//          .when()
//          .put(ID_COMMENT_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(Comment.class);
//
//    assertEquals(response.getBody(), COMMENT_TEXT);
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldDeleteComment() {
//
//    Comment comment = createComment(user, requisition, COMMENT);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", comment.getId())
//          .when()
//          .delete(ID_COMMENT_URL)
//          .then()
//          .statusCode(204);
//
//    assertFalse(commentRepository.exists(requisition.getId()));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotDeleteNonexistentComment() {
//
//    Comment comment = createComment(user, requisition, COMMENT);
//    commentRepository.delete(comment);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", comment.getId())
//          .when()
//          .delete(ID_COMMENT_URL)
//          .then()
//          .statusCode(404);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldApproveAuthorizedRequisition() {
//    requisition.setStatus(RequisitionStatus.AUTHORIZED);
//    requisitionRepository.save(requisition);
//    testApproveRequisition(requisition);
//  }
//
//  @Test
//  public void shouldApproveSubmittedRequisitionIfSkippedAuthorization() {
//    configurationSettingRepository.save(new ConfigurationSetting("skipAuthorization", "true"));
//    requisition.setStatus(RequisitionStatus.SUBMITTED);
//    requisitionRepository.save(requisition);
//    testApproveRequisition(requisition);
//  }
//
//  @Test
//  public void shouldInitializeRequisition() {
//
//    requisitionRepository.delete(requisition);
//
//    restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .body(requisition)
//            .when()
//            .post(INITIATE_URL)
//            .then()
//            .statusCode(201);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldGetSubmittedRequisitions() {
//
//    requisition.setStatus(RequisitionStatus.SUBMITTED);
//    requisitionRepository.save(requisition);
//
//    Requisition[] response = restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .when()
//            .get(SUBMITTED_URL)
//            .then()
//            .statusCode(200)
//            .extract().as(Requisition[].class);
//
//    Iterable<Requisition> requisitions = Arrays.asList(response);
//    assertTrue(requisitions.iterator().hasNext());
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldAuthorizeRequisition() {
//
//    requisition.setStatus(RequisitionStatus.SUBMITTED);
//    requisitionRepository.save(requisition);
//
//    restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .body(requisition)
//            .pathParam("id", requisition.getId())
//            .when()
//            .put(AUTHORIZATION_URL)
//            .then()
//            .statusCode(200);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotAuthorizeIfSkippedAuthorization() {
//    configurationSettingRepository.save(new ConfigurationSetting("skipAuthorization", "true"));
//
//    requisition.setStatus(RequisitionStatus.SUBMITTED);
//    requisitionRepository.save(requisition);
//
//    restAssured.given()
//            .queryParam(ACCESS_TOKEN, getToken())
//            .contentType(MediaType.APPLICATION_JSON_VALUE)
//            .body(requisition)
//            .pathParam("id", requisition.getId())
//            .when()
//            .put(AUTHORIZATION_URL)
//            .then()
//            .statusCode(400);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldGetChosenRequisition() {
//
//    Requisition response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisition.getId())
//          .when()
//          .get(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(Requisition.class);
//
//    assertTrue(requisitionRepository.exists(response.getId()));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotGetNonexistentRequisition() {
//
//    requisitionRepository.delete(requisition);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisition.getId())
//          .when()
//          .get(ID_URL)
//          .then()
//          .statusCode(404);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldUpdateRequisitionIfStatusIsInitiated() {
//
//    requisition.setEmergency(true);
//
//    Requisition response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisition.getId())
//          .body(requisition)
//          .when()
//          .put(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(Requisition.class);
//
//    assertTrue(response.getEmergency());
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotUpdateRequisitionIfStatusIsNotInitiated() {
//
//    requisition.setStatus(RequisitionStatus.SUBMITTED);
//    requisitionRepository.save(requisition);
//    requisition.setEmergency(true);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisition.getId())
//          .body(requisition)
//          .when()
//          .put(ID_URL)
//          .then()
//          .statusCode(400);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  private void testApproveRequisition(Requisition requisition) {
//
//    Requisition response = restAssured.given()
//        .queryParam(ACCESS_TOKEN, getToken())
//        .contentType(MediaType.APPLICATION_JSON_VALUE)
//        .pathParam("id", requisition.getId())
//        .when()
//        .put(APPROVE_REQUISITION)
//        .then()
//        .statusCode(200)
//        .extract().as(Requisition.class);
//
//    assertNotNull(response.getId());
//    assertEquals(requisition.getId(), response.getId());
//    assertEquals(RequisitionStatus.APPROVED, response.getStatus());
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  private Requisition configureRequisition(Requisition requisition) {
//    requisition.setFacility(facility);
//    requisition.setProcessingPeriod(period);
//    requisition.setProgram(program);
//    requisition.setStatus(RequisitionStatus.INITIATED);
//    requisition.setSupervisoryNode(supervisoryNode);
//    requisition.setCreatedDate(localDateTime);
//    requisition.setEmergency(false);
//
//    return requisitionRepository.save(requisition);
//  }
//
//  private Comment createComment(UserDto author, Requisition req, String commentText) {
//    Comment comment = new Comment();
//    comment.setAuthorId(author.getId());
//    comment.setRequisition(req);
//    comment.setBody(commentText);
//    commentRepository.save(comment);
//    return comment;
//  }
//
//  private void generateRequisition(RequisitionStatus requisitionStatus, Facility facility) {
//    Requisition requisition = new Requisition();
//    requisition.setId(UUID.randomUUID());
//    requisition.setFacility(facility);
//    requisition.setProcessingPeriod(generatePeriod());
//    requisition.setProgram(generateProgram());
//    requisition.setCreatedDate(LocalDateTime.now());
//    requisition.setStatus(requisitionStatus);
//    requisition.setEmergency(true);
//    requisitionRepository.save(requisition);
//  }
//
//  private ProcessingPeriod generatePeriod() {
//    ProcessingPeriod period = new ProcessingPeriod();
//    Integer instanceNumber = this.getNextInstanceNumber();
//    period.setName("PeriodName" + instanceNumber);
//    period.setDescription("PeriodDescription" + instanceNumber);
//    period.setEndDate(LocalDate.now().plusDays(instanceNumber % 28));
//    period.setStartDate(LocalDate.now().minusDays(instanceNumber % 28));
//    period.setProcessingSchedule(generateSchedule());
//    periodRepository.save(period);
//    return period;
//  }
//
//  private ProcessingSchedule generateSchedule() {
//    ProcessingSchedule schedule = new ProcessingSchedule();
//    Integer instanceNumber = this.getNextInstanceNumber();
//    schedule.setCode("ScheduleCode" + instanceNumber);
//    schedule.setName("ScheduleName" + instanceNumber);
//    schedule.setModifiedDate(LocalDateTime.now().minusDays(instanceNumber % 28));
//    scheduleRepository.save(schedule);
//    return schedule;
//  }
//
//  private Program generateProgram() {
//    Program program = new Program();
//    Integer instanceNumber = this.getNextInstanceNumber();
//    program.setCode("ProgramCode" + instanceNumber);
//    program.setName("ProgramName" + instanceNumber);
//    program.setPeriodsSkippable(false);
//    programRepository.save(program);
//    return program;
//  }
//
//  private Facility generateFacility(String name) {
//    Integer instanceNumber = this.getNextInstanceNumber();
//    GeographicZone geographicZone = generateGeographicZone();
//    FacilityType facilityType = generateFacilityType();
//    Facility facility = new Facility();
//    facility.setType(facilityType);
//    facility.setGeographicZone(geographicZone);
//    facility.setCode("FacilityCode" + instanceNumber);
//    facility.setName(name);
//    facility.setDescription("FacilityDescription" + instanceNumber);
//    facility.setActive(true);
//    facility.setEnabled(true);
//    facilityRepository.save(facility);
//    return facility;
//  }
//
//  private GeographicLevel generateGeographicLevel() {
//    GeographicLevel geographicLevel = new GeographicLevel();
//    geographicLevel.setCode("GeographicLevel" + this.getNextInstanceNumber());
//    geographicLevel.setLevelNumber(1);
//    geographicLevelRepository.save(geographicLevel);
//    return geographicLevel;
//  }
//
//  private GeographicZone generateGeographicZone() {
//    GeographicZone geographicZone = new GeographicZone();
//    geographicZone.setCode("GeographicZone" + this.getNextInstanceNumber());
//    geographicZone.setLevel(generateGeographicLevel());
//    geographicZoneRepository.save(geographicZone);
//    return geographicZone;
//  }
//
//  private FacilityType generateFacilityType() {
//    FacilityType facilityType = new FacilityType();
//    facilityType.setCode("FacilityType" + this.getNextInstanceNumber());
//    facilityTypeRepository.save(facilityType);
//    return facilityType;
//  }
//
//  private void generateRequisitions() {
//    for (int i = 0; i < 4; i++) {
//      Facility facility1 = generateFacility("FacilityNameA");
//      Facility facility2 = generateFacility("FacilityName" + valueOf(i));
//      generateRequisition(RequisitionStatus.APPROVED, facility2);
//      generateRequisition(RequisitionStatus.SUBMITTED, facility1);
//      for (int j = 0; j < 4; j++) {
//        generateRequisition(RequisitionStatus.APPROVED, facility1);
//      }
//    }
//  }
//
//  @Test
//  public void shouldGetApprovedRequisitionsWithSortByAscendingFilterByAndPaging() {
//    generateRequisitions();
//    Integer pageSize = 10;
//    String filterValue = "facilityNameA";
//
//    Requisition[] response = restAssured.given()
//        .queryParam(ACCESS_TOKEN, getToken())
//        .queryParam("filterValue", filterValue)
//        .queryParam("filterBy", "facilityName")
//        .queryParam("sortBy", "facilityCode")
//        .queryParam("descending", Boolean.FALSE.toString())
//        .queryParam("pageNumber", valueOf(2))
//        .queryParam("pageSize", valueOf(pageSize))
//        .contentType(MediaType.APPLICATION_JSON_VALUE)
//        .when()
//        .get(APPROVED_REQUISITIONS_SEARCH_URL)
//        .then()
//        .statusCode(200)
//        .extract().as(Requisition[].class);
//
//    List<Requisition> requisitions = Arrays.asList(response);
//    Iterator<Requisition> requisitionIterator = requisitions.iterator();
//
//    Assert.assertTrue(requisitions.size() <= pageSize);
//    Requisition requisition1 = null;
//    if (requisitionIterator.hasNext()) {
//      requisition1 = requisitionIterator.next();
//    }
//    Requisition requisition2;
//    while (requisitionIterator.hasNext()) {
//      requisition2 = requisitionIterator.next();
//
//      RequisitionStatus requisitionStatus = requisition1.getStatus();
//      Assert.assertTrue(requisitionStatus.equals(RequisitionStatus.APPROVED));
//
//      String facilityName = requisition1.getFacility().getName();
//      Assert.assertTrue(facilityName.contains(filterValue));
//
//      String facilityCode1 = requisition1.getFacility().getCode();
//      String facilityCode2 = requisition2.getFacility().getCode();
//      Assert.assertTrue(facilityCode1.compareTo(facilityCode2) <= 0);
//
//      if (facilityCode1.equals(facilityCode2)) {
//        LocalDateTime modifiedDate1 =
//            requisition1.getProcessingPeriod().getProcessingSchedule().getModifiedDate();
//        LocalDateTime modifiedDate2 =
//            requisition2.getProcessingPeriod().getProcessingSchedule().getModifiedDate();
//        Assert.assertTrue(modifiedDate1.isAfter(modifiedDate2));
//      }
//      requisition1 = requisition2;
//    }
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldGetApprovedRequisitionsWithSortByDescendingFilterByAndPaging() {
//    generateRequisitions();
//    Integer pageSize = 20;
//    String filterValue = "1";
//
//    Requisition[] response = restAssured.given()
//        .queryParam(ACCESS_TOKEN, getToken())
//        .queryParam("filterValue", filterValue)
//        .queryParam("filterBy", "facilityCode")
//        .queryParam("sortBy", "programName")
//        .queryParam("descending", Boolean.TRUE.toString())
//        .queryParam("pageNumber", valueOf(1))
//        .queryParam("pageSize", valueOf(pageSize))
//        .contentType(MediaType.APPLICATION_JSON_VALUE)
//        .when()
//        .get(APPROVED_REQUISITIONS_SEARCH_URL)
//        .then()
//        .statusCode(200)
//        .extract().as(Requisition[].class);
//
//    List<Requisition> requisitions = Arrays.asList(response);
//    Iterator<Requisition> requisitionIterator = requisitions.iterator();
//
//    Assert.assertTrue(requisitions.size() <= pageSize);
//    Requisition requisition1 = null;
//    if (requisitionIterator.hasNext()) {
//      requisition1 = requisitionIterator.next();
//    }
//    Requisition requisition2;
//    while (requisitionIterator.hasNext()) {
//      requisition2 = requisitionIterator.next();
//
//      RequisitionStatus requisitionStatus = requisition1.getStatus();
//      Assert.assertTrue(requisitionStatus.equals(RequisitionStatus.APPROVED));
//
//      String facilityCode = requisition1.getFacility().getCode();
//      Assert.assertTrue(facilityCode.contains(filterValue));
//
//      String programName1 = requisition1.getProgram().getName();
//      String programName2 = requisition2.getProgram().getName();
//      Assert.assertTrue(programName1.compareTo(programName2) >= 0);
//
//      if (programName1.equals(programName2)) {
//        LocalDate endDate1 = requisition1.getProcessingPeriod().getEndDate();
//        LocalDate endDate2 = requisition2.getProcessingPeriod().getEndDate();
//        Assert.assertTrue(endDate1.isAfter(endDate2));
//      }
//      requisition1 = requisition2;
//    }
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//}
