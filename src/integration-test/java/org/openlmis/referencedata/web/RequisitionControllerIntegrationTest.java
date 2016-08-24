package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
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
import org.openlmis.requisition.domain.Comment;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.CommentRepository;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String ACCESS_TOKEN = "access_token";
  private static final String REQUISITION_REPOSITORY_NAME = "RequisitionRepositoryIntegrationTest";
  private static final String EXPECTED_MESSAGE_FIRST_PART = "{\n  \"requisitionLines\" : ";
  private static final String RESOURCE_URL = "/api/requisitions";
  private static final String INSERT_COMMENT = RESOURCE_URL + "/{id}/comments";
  private static final String APPROVE_REQUISITION = RESOURCE_URL + "/{id}/approve";
  private static final String SKIP_URL = RESOURCE_URL + "/{id}/skip";
  private static final String REJECT_URL = RESOURCE_URL + "/{id}/reject";
  private static final String SUBMIT_URL = RESOURCE_URL + "/{id}/submit";
  private static final String SUBMITTED_URL = RESOURCE_URL + "/submitted";
  private static final String AUTHORIZATION_URL = RESOURCE_URL + "/{id}/authorize";
  private static final String ID_COMMENT_URL = RESOURCE_URL + "/comments/{id}";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String REQ_FOR_APPROVAL_URL = RESOURCE_URL + "/requisitions-for-approval";

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private RequisitionLineRepository requisitionLineRepository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  @Autowired
  private CommentRepository commentRepository;

  @Autowired
  private UserRepository userRepository;

  @Autowired
  private ConfigurationSettingRepository configurationSettingRepository;

  @Autowired
  private SupervisoryNodeRepository supervisoryNodeRepository;

  private RequisitionLine requisitionLine = new RequisitionLine();
  private Requisition requisition = new Requisition();
  private Period period = new Period();
  private Product product = new Product();
  private Program program = new Program();
  private Facility facility = new Facility();
  private SupervisoryNode supervisoryNode = new SupervisoryNode();
  private User user;
  private LocalDateTime localDateTime = LocalDateTime.now();

  @Before
  public void setUp() {
    ProductCategory productCategory1 = new ProductCategory();
    productCategory1.setCode("PC1");
    productCategory1.setName("PC1 name");
    productCategory1.setDisplayOrder(1);
    productCategoryRepository.save(productCategory1);

    product.setCode(REQUISITION_REPOSITORY_NAME);
    product.setPrimaryName(REQUISITION_REPOSITORY_NAME);
    product.setDispensingUnit(REQUISITION_REPOSITORY_NAME);
    product.setDosesPerDispensingUnit(10);
    product.setPackSize(1);
    product.setPackRoundingThreshold(0);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(true);
    product.setTracer(false);
    product.setProductCategory(productCategory1);
    productRepository.save(product);

    program.setCode(REQUISITION_REPOSITORY_NAME);
    program.setPeriodsSkippable(true);
    programRepository.save(program);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode(REQUISITION_REPOSITORY_NAME);
    facilityTypeRepository.save(facilityType);

    GeographicLevel level = new GeographicLevel();
    level.setCode(REQUISITION_REPOSITORY_NAME);
    level.setLevelNumber(1);
    geographicLevelRepository.save(level);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(REQUISITION_REPOSITORY_NAME);
    geographicZone.setLevel(level);
    geographicZoneRepository.save(geographicZone);

    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(REQUISITION_REPOSITORY_NAME);
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    Schedule schedule = new Schedule();
    schedule.setCode(REQUISITION_REPOSITORY_NAME);
    schedule.setName(REQUISITION_REPOSITORY_NAME);
    scheduleRepository.save(schedule);

    period.setName(REQUISITION_REPOSITORY_NAME);
    period.setProcessingSchedule(schedule);
    period.setDescription(REQUISITION_REPOSITORY_NAME);
    period.setStartDate(LocalDate.of(2016, 1, 1));
    period.setEndDate(LocalDate.of(2016, 2, 1));
    periodRepository.save(period);

    requisition.setFacility(facility);
    requisition.setProcessingPeriod(period);
    requisition.setProgram(program);
    requisition.setStatus(RequisitionStatus.INITIATED);

    requisitionRepository.save(requisition);

    requisitionLine.setProduct(product);
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    List<RequisitionLine> requisitionLines = new ArrayList<>();
    requisitionLines.add(requisitionLine);

    user = userRepository.findOne(INITIAL_USER_ID);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);

    supervisoryNode.setName("name");
    supervisoryNode.setCode("code");
    supervisoryNode.setDescription("description");
    supervisoryNode.setFacility(facility);
    supervisoryNodeRepository.save(supervisoryNode);
    requisition.setSupervisoryNode(supervisoryNode);
    requisition.setCreatedDate(localDateTime);
    requisitionRepository.save(requisition);
  }

  @Test
  public void shouldFindRequisitions() {
    Requisition[] response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .queryParam("program", program.getId())
            .queryParam("processingPeriod", period.getId())
            .queryParam("facility", facility.getId())
            .queryParam("supervisoryNode", supervisoryNode.getId())
            .queryParam("requisitionStatus", RequisitionStatus.INITIATED)
            .queryParam("createdDateFrom", localDateTime.minusDays(2).toString())
            .queryParam("createdDateTo", localDateTime.plusDays(2).toString())
            .when()
            .get(SEARCH_URL)
            .then()
            .statusCode(200)
            .extract().as(Requisition[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertEquals(1, response.length);
    for ( Requisition receivedRequisition : response ) {
      assertEquals(
              receivedRequisition.getProgram().getId(),
              program.getId());
      assertEquals(
              receivedRequisition.getProcessingPeriod().getId(),
              period.getId());
      assertEquals(
              receivedRequisition.getFacility().getId(),
              facility.getId());
      assertEquals(
              receivedRequisition.getSupervisoryNode().getId(),
              supervisoryNode.getId());
      assertEquals(
              receivedRequisition.getStatus(),
              RequisitionStatus.INITIATED);
      assertTrue(
              receivedRequisition.getCreatedDate().isBefore(localDateTime.plusDays(2)));
      assertTrue(
              receivedRequisition.getCreatedDate().isAfter(localDateTime.minusDays(2)));
    }
  }

  @Test
  public void shouldSubmitCorrectRequisition() {

    Requisition response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .body(requisition)
            .when()
            .put(SUBMIT_URL)
            .then()
            .statusCode(200)
            .extract().as(Requisition.class);

    assertNotNull(response.getId());
    assertEquals(requisition.getId(), response.getId());
    assertEquals(RequisitionStatus.SUBMITTED, response.getStatus());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullRequisitionLines() {

    requisition.setRequisitionLines(null);
    requisition = requisitionRepository.save(requisition);

    String response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .body(requisition)
            .when()
            .put(SUBMIT_URL)
            .then()
            .statusCode(400)
            .extract().asString();

    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
            + "\"A requisitionLines must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullQuantityInRequisitionLine() {

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    List<RequisitionLine> requisitionLines = new ArrayList<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);

    String response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .body(requisition)
            .when()
            .put(SUBMIT_URL)
            .then()
            .statusCode(400)
            .extract().asString();

    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
            + "\"A quantity must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullBeginningBalanceInRequisitionLine() {

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    List<RequisitionLine> requisitionLines = new ArrayList<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);

    String response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .body(requisition)
            .when()
            .put(SUBMIT_URL)
            .then()
            .statusCode(400)
            .extract().asString();

    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
            + "\"A beginning balance must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNegativeBeginningBalanceInRequisitionLine() {

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setBeginningBalance(-1);
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    List<RequisitionLine> requisitionLines = new ArrayList<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);

    String response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .body(requisition)
            .when()
            .put(SUBMIT_URL)
            .then()
            .statusCode(400)
            .extract().asString();

    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
            + "\"A beginning balance must be a non-negative value.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullTotalReceivedQuantityInRequisitionLine() {

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    List<RequisitionLine> requisitionLines = new ArrayList<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);

    String response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .body(requisition)
            .when()
            .put(SUBMIT_URL)
            .then()
            .statusCode(400)
            .extract().asString();

    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
            + "\"A total received quantity"
            + " must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNegativeTotalReceivedQuantityInRequisitionLine() {

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setTotalReceivedQuantity(-1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    List<RequisitionLine> requisitionLines = new ArrayList<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);

    String response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .body(requisition)
            .when()
            .put(SUBMIT_URL)
            .then()
            .statusCode(400)
            .extract().asString();

    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
            + "\"A total received quantity must be a non-negative value.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullStockHandInRequisitionLine() {

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setProduct(product);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    List<RequisitionLine> requisitionLines = new ArrayList<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);

    String response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .body(requisition)
            .when()
            .put(SUBMIT_URL)
            .then()
            .statusCode(400)
            .extract().asString();

    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
            + "\"A total stock on hand must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullConsumedQuantityInRequisitionLine() {

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setProduct(product);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLine.setStockOnHand(1);
    requisitionLineRepository.save(requisitionLine);

    List<RequisitionLine> requisitionLines = new ArrayList<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);

    String response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .body(requisition)
            .when()
            .put(SUBMIT_URL)
            .then()
            .statusCode(400)
            .extract().asString();

    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
            + "\"A total consumed quantity"
            + " must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullAttributesInRequisitionLine() {

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(null);
    requisitionLine.setTotalConsumedQuantity(null);
    requisitionLine.setBeginningBalance(null);
    requisitionLine.setTotalReceivedQuantity(null);
    requisitionLine.setTotalLossesAndAdjustments(null);
    requisitionLineRepository.save(requisitionLine);

    List<RequisitionLine> requisitionLines = new ArrayList<>();
    requisitionLines.add(requisitionLine);

    requisition.setRequisitionLines(requisitionLines);
    requisition = requisitionRepository.save(requisition);

    String response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .body(requisition)
            .when()
            .put(SUBMIT_URL)
            .then()
            .statusCode(400)
            .extract().asString();

    String expectedExceptionMessage = EXPECTED_MESSAGE_FIRST_PART
            + "\"A total losses and adjustments must be entered prior "
            + "to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldSkipRequisition() {
    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .when()
            .put(SKIP_URL)
            .then()
            .statusCode(200);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldRejectRequisition() {

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .when()
            .put(REJECT_URL)
            .then()
            .statusCode(200);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotRejectWithWrongStatus() {

    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .when()
            .put(REJECT_URL)
            .then()
            .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldDeleteRequisition() {

    requisition.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition);

    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .when()
            .delete(ID_URL)
            .then()
            .statusCode(204);

    assertFalse(requisitionRepository.exists(requisition.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteRequisitionWithWrongStatus() {

    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);

    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .when()
            .delete(ID_URL)
            .then()
            .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private Comment createComment(User author, Requisition req, String commentText) {
    Comment comment = new Comment();
    comment.setAuthor(author);
    comment.setRequisition(req);
    comment.setBody(commentText);
    commentRepository.save(comment);
    return comment;
  }

  @Test
  public void shouldGetCommentsForRequisition() {
    createComment(user, requisition, "First comment");
    createComment(user, requisition, "Second comment");

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    Comment[] response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .when()
            .get(INSERT_COMMENT)
            .then()
            .statusCode(200)
            .extract().as(Comment[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    List<Comment> commentList = Arrays.asList(response);
    assertEquals("First comment", commentList.get(0).getBody());
    assertEquals("Second comment", commentList.get(1).getBody());
  }

  @Test
  public void shouldGetRequisitionsForApprovalForSpecificUser() {
    requisition.setSupervisoryNode(supervisoryNode);
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    user.setSupervisedNode(supervisoryNode);
    userRepository.save(user);

    Requisition[] response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(REQ_FOR_APPROVAL_URL)
        .then()
        .statusCode(200)
        .extract().as(Requisition[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    List<Requisition> responseList = Arrays.asList(response);
    List<Requisition> expectedRequisitionList = new ArrayList<>();
    expectedRequisitionList.add(requisition);

    for (int i = 0; i < responseList.size(); i++) {
      assertEquals(expectedRequisitionList.get(i).getId(), responseList.get(i).getId());
    }
    user.setSupervisedNode(null);
    userRepository.save(user);
  }

  @Test
  public void shouldInsertComment() {

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    createComment(user, requisition, "Previous comment");
    Comment userPostComment = new Comment();
    userPostComment.setBody("User comment");

    Comment[] response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .body(userPostComment)
            .pathParam("id", requisition.getId())
            .when()
            .post(INSERT_COMMENT)
            .then()
            .statusCode(200)
            .extract().as(Comment[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    List<Comment> commentList = Arrays.asList(response);
    assertEquals("Previous comment", commentList.get(0).getBody());
    assertEquals("User comment", commentList.get(1).getBody());
  }

  @Test
  public void shouldGetChosenComment() {

    Comment comment = createComment(user, requisition, "Comment");

    Comment response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", comment.getId())
          .when()
          .get(ID_COMMENT_URL)
          .then()
          .statusCode(200)
          .extract().as(Comment.class);

    assertTrue(commentRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateComment() {

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    Comment comment = createComment(user, requisition, "Comment");
    comment.setBody("OpenLMIS");

    Comment response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", comment.getId())
          .body(comment)
          .when()
          .put(ID_COMMENT_URL)
          .then()
          .statusCode(200)
          .extract().as(Comment.class);

    assertEquals(response.getBody(), "OpenLMIS");
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldDeleteComment() {

    Comment comment = createComment(user, requisition, "Comment");

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", comment.getId())
          .when()
          .delete(ID_COMMENT_URL)
          .then()
          .statusCode(204);

    assertFalse(commentRepository.exists(requisition.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private void testApproveRequisition(Requisition requisition) {

    Requisition response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .when()
            .put(APPROVE_REQUISITION)
            .then()
            .statusCode(200)
            .extract().as(Requisition.class);

    assertNotNull(response.getId());
    assertEquals(requisition.getId(), response.getId());
    assertEquals(RequisitionStatus.APPROVED, response.getStatus());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldApproveAuthorizedRequisition() {
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);
    testApproveRequisition(requisition);
  }

  @Test
  public void shouldApproveSubmittedRequisitionIfSkippedAuthorization() {
    configurationSettingRepository.save(new ConfigurationSetting("skipAuthorization", "true"));
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);
    testApproveRequisition(requisition);
  }

  @Test
  public void shouldInitializeRequisition() {

    requisitionRepository.delete(requisition);

    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .body(requisition)
            .when()
            .post(RESOURCE_URL)
            .then()
            .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetSubmittedRequisitions() {

    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);

    Requisition[] response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .when()
            .get(SUBMITTED_URL)
            .then()
            .statusCode(200)
            .extract().as(Requisition[].class);

    Iterable<Requisition> requisitions = Arrays.asList(response);
    assertTrue(requisitions.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldAuthorizeRequisition() {

    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);

    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .body(requisition)
            .pathParam("id", requisition.getId())
            .when()
            .put(AUTHORIZATION_URL)
            .then()
            .statusCode(200);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotAuthorizeIfSkippedAuthorization() {
    configurationSettingRepository.save(new ConfigurationSetting("skipAuthorization", "true"));

    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);

    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .body(requisition)
            .pathParam("id", requisition.getId())
            .when()
            .put(AUTHORIZATION_URL)
            .then()
            .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllRequisitions() {

    Requisition[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(Requisition[].class);

    Iterable<Requisition> requisition = Arrays.asList(response);
    assertTrue(requisition.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenRequisition() {

    Requisition response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", requisition.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(Requisition.class);

    assertTrue(requisitionRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateRequisition() {

    requisition.setStatus(RequisitionStatus.APPROVED);

    Requisition response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", requisition.getId())
          .body(requisition)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(Requisition.class);

    assertEquals(response.getStatus(), RequisitionStatus.APPROVED);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
