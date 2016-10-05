package org.openlmis.requisition.web;

import com.google.common.collect.ImmutableMap;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.openlmis.requisition.domain.Comment;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.repository.CommentRepository;
import org.openlmis.requisition.repository.AvailableRequisitionColumnRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.UUID;

import guru.nidi.ramltester.junit.RamlMatchers;

import static java.lang.Integer.valueOf;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String ACCESS_TOKEN = "access_token";
  private static final String REQUISITION_REPOSITORY_NAME = "RequisitionRepositoryIntegrationTest";
  private static final String EXPECTED_MESSAGE_FIRST_PART = "{\n  \"requisitionLineItems\" : ";
  private static final String RESOURCE_URL = "/api/requisitions";
  private static final String INSERT_COMMENT = RESOURCE_URL + "/{id}/comments";
  private static final String INITIATE_URL = RESOURCE_URL + "/initiate";
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
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");
  private static final String COMMENT_TEXT = "OpenLMIS";
  private static final String COMMENT = "Comment";
  private static final String APPROVED_REQUISITIONS_SEARCH_URL =
      RESOURCE_URL + "/requisitions-for-convert";

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private CommentRepository commentRepository;

  @Autowired
  private ConfigurationSettingRepository configurationSettingRepository;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private PeriodReferenceDataService periodReferenceDataService;

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private AvailableRequisitionColumnRepository availableRequisitionColumnRepository;

  private RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
  private Requisition requisition = new Requisition();
  private ProcessingPeriodDto period = new ProcessingPeriodDto();
  private OrderableProductDto product = new OrderableProductDto();
  private ProgramDto program = new ProgramDto();
  private FacilityDto facility = new FacilityDto();
  private SupervisoryNodeDto supervisoryNode = new SupervisoryNodeDto();
  private UserDto user;
  private LocalDateTime localDateTime = LocalDateTime.now();

  @Before
  public void setUp() throws IOException {
    user = new UserDto();
    user.setId(INITIAL_USER_ID);
    user.setUsername("admin");
    user.setFirstName("Admin");
    user.setLastName("User");
    user.setEmail("example@mail.com");

    product.setId(UUID.randomUUID());

    program.setId(UUID.randomUUID());
    program.setCode(REQUISITION_REPOSITORY_NAME);
    program.setPeriodsSkippable(true);

    facility.setId(UUID.randomUUID());
    facility.setCode(REQUISITION_REPOSITORY_NAME);
    facility.setActive(true);
    facility.setEnabled(true);

    ProcessingScheduleDto processingScheduleDto = new ProcessingScheduleDto();
    processingScheduleDto.setId(getProcessingScheduleId());
    processingScheduleDto.setCode("Schedule Code");
    processingScheduleDto.setName("Schedule Name");

    period.setId(getProcessingPeriodId());
    period.setName("Period Name");
    period.setProcessingSchedule(processingScheduleDto);
    period.setDescription("Period Description");
    period.setStartDate(LocalDate.of(2016, 3, 1));
    period.setEndDate(LocalDate.of(2017, 3, 1));

    supervisoryNode.setId(UUID.randomUUID());
    supervisoryNode.setName("name");
    supervisoryNode.setCode("code");
    supervisoryNode.setDescription("description");
    supervisoryNode.setFacility(facility);

    configureRequisition(requisition);

    requisitionLineItem.setOrderableProduct(product.getId());
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setStockOnHand(1);
    requisitionLineItem.setTotalConsumedQuantity(1);
    requisitionLineItem.setBeginningBalance(1);
    requisitionLineItem.setTotalReceivedQuantity(1);
    requisitionLineItem.setTotalLossesAndAdjustments(1);


    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(requisitionLineItem);

    requisition.setRequisitionLineItems(requisitionLineItems);
    requisition = requisitionRepository.save(requisition);
    requisitionRepository.save(requisition);

    RequisitionTemplateColumn templateColumn = new RequisitionTemplateColumn();
    templateColumn.setColumnDefinition(availableRequisitionColumnRepository.findOne(
        UUID.fromString("4a2e9fd3-1127-4b68-9912-84a5c00f6999")
    ));

    templateColumn.setName("Template Column");
    templateColumn.setIsDisplayed(true);

    RequisitionTemplate template = new RequisitionTemplate();
    template.setColumnsMap(ImmutableMap.of("beginningBalance", templateColumn));
    template.setProgram(program.getId());

    requisitionTemplateRepository.save(template);
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
              receivedRequisition.getProgram(),
              program.getId());
      assertEquals(
              receivedRequisition.getProcessingPeriod(),
              period.getId());
      assertEquals(
              receivedRequisition.getFacility(),
              facility.getId());
      assertEquals(
              receivedRequisition.getSupervisoryNode(),
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
  public void shouldNotSubmitRequisitionWithNullRequisitionLineItems() {

    requisition = configureRequisition(new Requisition());

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
            + "\"A requisitionLineItems must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullQuantityInRequisitionLineItem() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setOrderableProduct(product.getId());
    requisitionLineItem.setStockOnHand(1);
    requisitionLineItem.setTotalConsumedQuantity(1);
    requisitionLineItem.setBeginningBalance(1);
    requisitionLineItem.setTotalReceivedQuantity(1);
    requisitionLineItem.setTotalLossesAndAdjustments(1);

    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(requisitionLineItem);

    requisition.setRequisitionLineItems(requisitionLineItems);
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
            + "\"Quantity must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullBeginningBalanceInRequisitionLineItem() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setOrderableProduct(product.getId());
    requisitionLineItem.setStockOnHand(1);
    requisitionLineItem.setTotalConsumedQuantity(1);
    requisitionLineItem.setTotalReceivedQuantity(1);
    requisitionLineItem.setTotalLossesAndAdjustments(1);

    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(requisitionLineItem);

    requisition.setRequisitionLineItems(requisitionLineItems);
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
            + "\"Beginning balance must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNegativeBeginningBalanceInRequisitionLineItem() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setBeginningBalance(-1);
    requisitionLineItem.setOrderableProduct(product.getId());
    requisitionLineItem.setStockOnHand(1);
    requisitionLineItem.setTotalConsumedQuantity(1);
    requisitionLineItem.setTotalReceivedQuantity(1);
    requisitionLineItem.setTotalLossesAndAdjustments(1);

    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(requisitionLineItem);

    requisition.setRequisitionLineItems(requisitionLineItems);
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
            + "\"Beginning balance must be a non-negative value.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullTotalReceivedQuantityInRequisitionLineItem() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setOrderableProduct(product.getId());
    requisitionLineItem.setStockOnHand(1);
    requisitionLineItem.setTotalConsumedQuantity(1);
    requisitionLineItem.setTotalLossesAndAdjustments(1);

    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(requisitionLineItem);

    requisition.setRequisitionLineItems(requisitionLineItems);
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
            + "\"Total received quantity"
            + " must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNegativeTotalReceivedQuantityInRequisitionLineItem() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setBeginningBalance(1);
    requisitionLineItem.setOrderableProduct(product.getId());
    requisitionLineItem.setStockOnHand(1);
    requisitionLineItem.setTotalConsumedQuantity(1);
    requisitionLineItem.setTotalReceivedQuantity(-1);
    requisitionLineItem.setTotalLossesAndAdjustments(1);

    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(requisitionLineItem);

    requisition.setRequisitionLineItems(requisitionLineItems);
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
            + "\"Total received quantity must be a non-negative value.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullStockHandInRequisitionLineItem() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setBeginningBalance(1);
    requisitionLineItem.setOrderableProduct(product.getId());
    requisitionLineItem.setTotalConsumedQuantity(1);
    requisitionLineItem.setTotalReceivedQuantity(1);
    requisitionLineItem.setTotalLossesAndAdjustments(1);

    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(requisitionLineItem);

    requisition.setRequisitionLineItems(requisitionLineItems);
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
           + "\"Total stock on hand must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullConsumedQuantityInRequisitionLineItem() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setBeginningBalance(1);
    requisitionLineItem.setOrderableProduct(product.getId());
    requisitionLineItem.setTotalReceivedQuantity(1);
    requisitionLineItem.setTotalLossesAndAdjustments(1);
    requisitionLineItem.setStockOnHand(1);

    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(requisitionLineItem);

    requisition.setRequisitionLineItems(requisitionLineItems);
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
            + "\"Total consumed quantity"
            + " must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullAttributesInRequisitionLineItem() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setOrderableProduct(product.getId());
    requisitionLineItem.setStockOnHand(null);
    requisitionLineItem.setTotalConsumedQuantity(null);
    requisitionLineItem.setBeginningBalance(null);
    requisitionLineItem.setTotalReceivedQuantity(null);
    requisitionLineItem.setTotalLossesAndAdjustments(null);

    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(requisitionLineItem);

    requisition.setRequisitionLineItems(requisitionLineItems);
    requisition = requisitionRepository.save(requisition);

    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", requisition.getId())
            .body(requisition)
            .when()
            .put(SUBMIT_URL)
            .then()
            .statusCode(400)
            .extract().asString();

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
  public void shouldNotDeleteNonexistentRequisition() {

    requisitionRepository.delete(requisition);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", requisition.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(404);

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
    requisition.setSupervisoryNode(supervisoryNode.getId());
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    //user.setSupervisedNode(supervisoryNode.getId());

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
    //user.setSupervisedNode(null);
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

    Comment comment = createComment(user, requisition, COMMENT);

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
  public void shouldNotGetNonexistentComment() {

    Comment comment = createComment(user, requisition, COMMENT);
    commentRepository.delete(comment);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", comment.getId())
          .when()
          .get(ID_COMMENT_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewCommentIfDoesNotExist() {

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    Comment comment = new Comment();
    comment.setBody(COMMENT_TEXT);
    comment.setAuthorId(user.getId());
    comment.setRequisition(requisition);

    Comment response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", ID)
          .body(comment)
          .when()
          .put(ID_COMMENT_URL)
          .then()
          .statusCode(200)
          .extract().as(Comment.class);

    assertEquals(response.getBody(), COMMENT_TEXT);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateComment() {

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    Comment comment = createComment(user, requisition, COMMENT);
    comment.setBody(COMMENT_TEXT);

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

    assertEquals(response.getBody(), COMMENT_TEXT);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldDeleteComment() {

    Comment comment = createComment(user, requisition, COMMENT);

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

  @Test
  public void shouldNotDeleteNonexistentComment() {

    Comment comment = createComment(user, requisition, COMMENT);
    commentRepository.delete(comment);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", comment.getId())
          .when()
          .delete(ID_COMMENT_URL)
          .then()
          .statusCode(404);

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

  @Ignore
  @Test
  public void shouldInitializeRequisition() {

    requisitionRepository.delete(requisition);

    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .queryParam("program", program.getId())
            .queryParam("facility", facility.getId())
            .queryParam("suggestedPeriod", period.getId())
            .queryParam("emergency", false)
            .when()
            .post(INITIATE_URL)
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
  public void shouldNotGetNonexistentRequisition() {

    requisitionRepository.delete(requisition);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", requisition.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateRequisitionIfStatusIsInitiated() {

    requisition.setEmergency(true);

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

    assertTrue(response.getEmergency());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotUpdateRequisitionIfStatusIsNotInitiated() {

    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);
    requisition.setEmergency(true);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", requisition.getId())
          .body(requisition)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(400);

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

  private Requisition configureRequisition(Requisition requisition) {
    requisition.setFacility(facility.getId());
    requisition.setProcessingPeriod(period.getId());
    requisition.setProgram(program.getId());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setSupervisoryNode(supervisoryNode.getId());
    requisition.setCreatedDate(localDateTime);
    requisition.setEmergency(false);

    return requisitionRepository.save(requisition);
  }

  private Comment createComment(UserDto author, Requisition req, String commentText) {
    Comment comment = new Comment();
    comment.setAuthorId(author.getId());
    comment.setRequisition(req);
    comment.setBody(commentText);
    commentRepository.save(comment);
    return comment;
  }

  private void generateRequisition(RequisitionStatus requisitionStatus, UUID facility) {
    Requisition requisition = new Requisition();
    requisition.setId(UUID.randomUUID());
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(UUID.randomUUID());
    requisition.setProgram(UUID.randomUUID());
    requisition.setCreatedDate(LocalDateTime.now());
    requisition.setStatus(requisitionStatus);
    requisition.setEmergency(true);
    requisitionRepository.save(requisition);
  }

  private void generateRequisitions() {
    for (int i = 0; i < 4; i++) {
      UUID facility1 = UUID.randomUUID();
      UUID facility2 = UUID.randomUUID();
      generateRequisition(RequisitionStatus.APPROVED, facility2);
      generateRequisition(RequisitionStatus.SUBMITTED, facility1);
      for (int j = 0; j < 4; j++) {
        generateRequisition(RequisitionStatus.APPROVED, facility1);
      }
    }
  }

  @Ignore
  @Test
  public void shouldGetApprovedRequisitionsWithSortByAscendingFilterByAndPaging() {
    generateRequisitions();
    Integer pageSize = 10;
    String filterValue = "facilityNameA";

    Requisition[] response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam("filterValue", filterValue)
        .queryParam("filterBy", "facilityName")
        .queryParam("sortBy", "facilityCode")
        .queryParam("descending", Boolean.FALSE.toString())
        .queryParam("pageNumber", valueOf(2))
        .queryParam("pageSize", pageSize)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(Requisition[].class);

    List<Requisition> requisitions = Arrays.asList(response);
    Iterator<Requisition> requisitionIterator = requisitions.iterator();

    Assert.assertTrue(requisitions.size() <= pageSize);
    Requisition requisition1 = null;
    if (requisitionIterator.hasNext()) {
      requisition1 = requisitionIterator.next();
    }
    Requisition requisition2;
    while (requisitionIterator.hasNext()) {
      requisition2 = requisitionIterator.next();

      RequisitionStatus requisitionStatus = requisition1.getStatus();
      Assert.assertTrue(requisitionStatus.equals(RequisitionStatus.APPROVED));

      UUID facility1Id = requisition1.getFacility();
      FacilityDto facility1 = facilityReferenceDataService.findOne(facility1Id);

      UUID facility2Id = requisition1.getFacility();
      FacilityDto facility2 = facilityReferenceDataService.findOne(facility2Id);

      Assert.assertNotNull(facility1);
      Assert.assertNotNull(facility2);
      Assert.assertTrue(facility1.getName().contains(filterValue));

      Assert.assertTrue(facility1.getCode()
          .compareTo(facility2.getCode()) <= 0);

      if (facility1.getCode().equals(facility2.getCode())) {
        UUID processingPeriodId1 = requisition1.getProcessingPeriod();
        ProcessingPeriodDto processingPeriodDto1 =
            periodReferenceDataService.findOne(processingPeriodId1);

        UUID processingPeriodId2 = requisition2.getProcessingPeriod();
        ProcessingPeriodDto processingPeriodDto2 =
            periodReferenceDataService.findOne(processingPeriodId2);

        LocalDateTime modifiedDate1 =
            processingPeriodDto1.getProcessingSchedule().getModifiedDate();
        LocalDateTime modifiedDate2 =
            processingPeriodDto2.getProcessingSchedule().getModifiedDate();
        Assert.assertTrue(modifiedDate1.isAfter(modifiedDate2));
      }
      requisition1 = requisition2;
    }
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Ignore
  @Test
  public void shouldGetApprovedRequisitionsWithSortByDescendingFilterByAndPaging() {
    generateRequisitions();
    Integer pageSize = 20;
    String filterValue = "1";

    Requisition[] response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam("filterValue", filterValue)
        .queryParam("filterBy", "facilityCode")
        .queryParam("sortBy", "programName")
        .queryParam("descending", Boolean.TRUE.toString())
        .queryParam("pageNumber", valueOf(1))
        .queryParam("pageSize", pageSize)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(Requisition[].class);

    List<Requisition> requisitions = Arrays.asList(response);
    Iterator<Requisition> requisitionIterator = requisitions.iterator();

    Assert.assertTrue(requisitions.size() <= pageSize);
    Requisition requisition1 = null;
    if (requisitionIterator.hasNext()) {
      requisition1 = requisitionIterator.next();
    }
    Requisition requisition2;
    while (requisitionIterator.hasNext()) {
      requisition2 = requisitionIterator.next();

      RequisitionStatus requisitionStatus = requisition1.getStatus();
      Assert.assertTrue(requisitionStatus.equals(RequisitionStatus.APPROVED));

      UUID facilitId1 = requisition1.getFacility();
      FacilityDto facility1 = facilityReferenceDataService.findOne(facilitId1);

      String facilityCode = facility1.getCode();
      Assert.assertTrue(facilityCode.contains(filterValue));

      UUID programId1 = requisition1.getProgram();
      ProgramDto program1 = programReferenceDataService.findOne(programId1);

      UUID programId2 = requisition2.getProgram();
      ProgramDto program2 = programReferenceDataService.findOne(programId2);

      Assert.assertTrue(program1.getName().compareTo(program2.getName()) >= 0);

      if (program1.getCode().equals(program2.getName())) {
        UUID periodId1 = requisition1.getProcessingPeriod();
        ProcessingPeriodDto period1 = periodReferenceDataService.findOne(periodId1);

        UUID periodId2 = requisition2.getProcessingPeriod();
        ProcessingPeriodDto period2 = periodReferenceDataService.findOne(periodId2);
        Assert.assertTrue(period1.getEndDate().isAfter(period2.getEndDate()));
      }
      requisition1 = requisition2;
    }
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
