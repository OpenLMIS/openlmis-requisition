package org.openlmis.referencedata.web;

import com.fasterxml.jackson.core.JsonProcessingException;
import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.After;
import org.junit.Assert;
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
import org.openlmis.referencedata.domain.Comment;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.CommentRepository;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.openlmis.referencedata.repository.GeographicLevelRepository;
import org.openlmis.referencedata.repository.GeographicZoneRepository;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.Assert.assertThat;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String ACCESS_TOKEN = "access_token";
  private static final String REQUISITION_REPOSITORY_NAME = "RequisitionRepositoryIntegrationTest";
  private static final String EXPECTED_MESSAGE_FIRST_PART = "{\n  \"requisitionLines\" : ";
  private static final String INSERT_COMMENT = "/api/requisitions/{id}/comments";
  private static final String APPROVE_REQUISITION =
          "/api/requisitions/{id}/approve";
  private static final String SKIP_URL = "/api/requisitions/{id}/skip";
  private static final String REJECT_URL = "/api/requisitions/{id}/reject";
  private static final String SUBMIT_URL = "/api/requisitions/{id}/submit";
  private static final String SUBMITTED_URL = "/api/requisitions/submitted";
  private static final String AUTHORIZATION_URL =
          "/api/requisitions/{id}/authorize";
  private static final String DELETE_URL = "/api/requisitions/{id}";
  private static final String SEARCH_URL = "/api/requisitions/search";
  private static final String INITIATE_URL = "/api/requisitions/initiate";

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

  private Requisition requisition = new Requisition();
  private Period period = new Period();
  private Product product = new Product();
  private Program program = new Program();
  private Facility facility = new Facility();
  private SupervisoryNode supervisoryNode = new SupervisoryNode();
  private User user;
  private LocalDateTime localDateTime = LocalDateTime.now();

  @Before
  public void setUp() throws JsonProcessingException {
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

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setProduct(product);
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
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

  @After
  public void cleanUp() {
    commentRepository.deleteAll();
    requisitionLineRepository.deleteAll();
    productRepository.deleteAll();
    requisitionRepository.deleteAll();
    programRepository.deleteAll();
    periodRepository.deleteAll();
    supervisoryNodeRepository.deleteAll();
    facilityRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    periodRepository.deleteAll();
    scheduleRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    productCategoryRepository.deleteAll();
    configurationSettingRepository.deleteAll();
  }

  @Test
  public void testSearchRequisitions() {
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
    Assert.assertEquals(1, response.length);
    for ( Requisition receivedRequisition : response ) {
      Assert.assertEquals(
              receivedRequisition.getProgram().getId(),
              program.getId());
      Assert.assertEquals(
              receivedRequisition.getProcessingPeriod().getId(),
              period.getId());
      Assert.assertEquals(
              receivedRequisition.getFacility().getId(),
              facility.getId());
      Assert.assertEquals(
              receivedRequisition.getSupervisoryNode().getId(),
              supervisoryNode.getId());
      Assert.assertEquals(
              receivedRequisition.getStatus(),
              RequisitionStatus.INITIATED);
      Assert.assertTrue(
              receivedRequisition.getCreatedDate().isBefore(localDateTime.plusDays(2)));
      Assert.assertTrue(
              receivedRequisition.getCreatedDate().isAfter(localDateTime.minusDays(2)));
    }
  }

  @Test
  public void testShouldSubmitCorrectRequisition() throws JsonProcessingException {

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

    Assert.assertNotNull(response.getId());
    Assert.assertEquals(requisition.getId(), response.getId());
    Assert.assertEquals(RequisitionStatus.SUBMITTED, response.getStatus());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNullRequisitionLines()
          throws JsonProcessingException {

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

    Assert.assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNullQuantityInRequisitionLine()
          throws JsonProcessingException {

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
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

    Assert.assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNullBeginningBalanceInRequisitionLine()
          throws JsonProcessingException {

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
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

    Assert.assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNegativeBeginningBalanceInRequisitionLine()
          throws JsonProcessingException {

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setBeginningBalance(-1);
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
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

    Assert.assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNullTotalReceivedQuantityInRequisitionLine()
          throws JsonProcessingException {

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
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

    Assert.assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNegativeTotalReceivedQuantityInRequisitionLine()
          throws JsonProcessingException {

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setTotalReceivedQuantity(-1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
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

    Assert.assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNullStockHandInRequisitionLine()
          throws JsonProcessingException {

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setProduct(product);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
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

    Assert.assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNullConsumedQuantityInRequisitionLinetest()
          throws JsonProcessingException {

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setProduct(product);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    requisitionLine.setStockOnHand(1);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
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

    Assert.assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldNotSubmitRequisitionWithNullAttributesInRequisitionLine()
          throws JsonProcessingException {

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setProduct(product);
    requisitionLine.setStockOnHand(null);
    requisitionLine.setTotalConsumedQuantity(null);
    requisitionLine.setBeginningBalance(null);
    requisitionLine.setTotalReceivedQuantity(null);
    requisitionLine.setTotalLossesAndAdjustments(null);
    requisitionLineRepository.save(requisitionLine);

    Set<RequisitionLine> requisitionLines = new HashSet<>();
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

    Assert.assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testSkip() throws JsonProcessingException {
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
  public void testReject() throws JsonProcessingException {

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
  public void testRejectWithBadStatus() throws JsonProcessingException {

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
  public void testDelete() {

    requisition.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", requisition.getId())
          .when()
          .delete(DELETE_URL)
          .then()
          .statusCode(204);

    Assert.assertFalse(requisitionRepository.exists(requisition.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testDeleteWithBadStatus() {

    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", requisition.getId())
          .when()
          .delete(DELETE_URL)
          .then()
          .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private void createComment(User author, Requisition req, String commentText) {
    Comment comment = new Comment();
    comment.setAuthor(author);
    comment.setRequisition(req);
    comment.setBody(commentText);
    commentRepository.save(comment);
  }

  @Test
  public void testGetCommentsForRequisition() {
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
    Assert.assertEquals("First comment", commentList.get(0).getBody());
    Assert.assertEquals("Second comment", commentList.get(1).getBody());
  }

  @Test
  public void testInsertComment() throws JsonProcessingException {

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
    Assert.assertEquals("Previous comment", commentList.get(0).getBody());
    Assert.assertEquals("User comment", commentList.get(1).getBody());
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

    Assert.assertNotNull(response.getId());
    Assert.assertEquals(requisition.getId(), response.getId());
    Assert.assertEquals(RequisitionStatus.APPROVED, response.getStatus());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testApproveRequisition() {
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);
    testApproveRequisition(requisition);
  }

  @Test
  public void testApproveRequisitionSkippedAuthorization() {
    configurationSettingRepository.save(new ConfigurationSetting("skipAuthorization", "true"));
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);
    testApproveRequisition(requisition);
  }

  @Test
  public void testInitializeRequisition() throws JsonProcessingException {

    requisitionRepository.delete(requisition);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(requisition)
          .when()
          .post(INITIATE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testGetSubmittedRequisitions() throws JsonProcessingException {

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
    Assert.assertTrue(requisitions.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testAuthorize() throws JsonProcessingException {

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
  public void testSkippedAuthorize() throws JsonProcessingException {
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
}
