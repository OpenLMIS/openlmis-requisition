package org.openlmis.requisition.web;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static java.lang.Integer.valueOf;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import com.github.tomakehurst.wiremock.client.ValueMatchingStrategy;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.Comment;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.repository.AvailableRequisitionColumnRepository;
import org.openlmis.requisition.repository.CommentRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import guru.nidi.ramltester.junit.RamlMatchers;

import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String ACCESS_TOKEN = "access_token";
  private static final String REQUISITION_REPOSITORY_NAME = "RequisitionRepositoryIntegrationTest";
  private static final String RESOURCE_URL = "/api/requisitions";
  private static final String INSERT_COMMENT = RESOURCE_URL + "/{id}/comments";
  private static final String INITIATE_URL = RESOURCE_URL + "/initiate";
  private static final String APPROVE_URL = RESOURCE_URL + "/{id}/approve";
  private static final String SKIP_URL = RESOURCE_URL + "/{id}/skip";
  private static final String REJECT_URL = RESOURCE_URL + "/{id}/reject";
  private static final String SUBMIT_URL = RESOURCE_URL + "/{id}/submit";
  private static final String SUBMITTED_URL = RESOURCE_URL + "/submitted";
  private static final String AUTHORIZATION_URL = RESOURCE_URL + "/{id}/authorize";
  private static final String ID_COMMENT_URL = RESOURCE_URL + "/comments/{id}";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String REQ_FOR_APPROVAL_URL = RESOURCE_URL + "/requisitionsForApproval";
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");
  private static final String COMMENT_TEXT = "OpenLMIS";
  private static final String COMMENT = "Comment";
  private static final String FACILITY = "facility";
  private static final String APPROVED_REQUISITIONS_SEARCH_URL =
      RESOURCE_URL + "/requisitionsForConvert";
  private static final UUID PERIOD_UUID = UUID.fromString("4c6b05c2-894b-11e6-ae22-56b6b6499611");
  private static final UUID PROGRAM_UUID = UUID.fromString("5c5a6f68-8658-11e6-ae22-56b6b6499611");
  private static final UUID FACILITY_UUID = UUID.fromString("1d5bdd9c-8702-11e6-ae22-56b6b6499611");
  private static final String PROGRAM = "program";

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private CommentRepository commentRepository;

  @Autowired
  private ConfigurationSettingRepository configurationSettingRepository;

  @Autowired
  private UserFulfillmentFacilitiesReferenceDataService fulfillmentFacilitiesReferenceDataService;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private AvailableRequisitionColumnRepository availableRequisitionColumnRepository;

  private RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
  private Requisition requisition = new Requisition();
  private Requisition requisitionForSearch = new Requisition();
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

    facility.setId(getSharedFacilityId());
    facility.setCode("facilityCode");
    facility.setActive(true);
    facility.setEnabled(true);

    ProcessingScheduleDto processingScheduleDto = new ProcessingScheduleDto();
    processingScheduleDto.setId(UUID.fromString("c73ad6a4-895c-11e6-ae22-56b6b6499611"));
    processingScheduleDto.setCode("Schedule Code");
    processingScheduleDto.setName("Schedule Name");

    period.setId(UUID.fromString("4c6b05c2-894b-11e6-ae22-56b6b6499611"));
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
    configureRequisitionForSearch(requisitionForSearch);

    requisitionLineItem.setOrderableProductId(product.getId());
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setRequestedQuantityExplanation("Requested Quantity Explanation");
    requisitionLineItem.setStockOnHand(2);
    requisitionLineItem.setTotalConsumedQuantity(1);
    requisitionLineItem.setBeginningBalance(1);
    requisitionLineItem.setTotalReceivedQuantity(1);
    requisitionLineItem.setTotalLossesAndAdjustments(1);
    requisitionLineItem.setApprovedQuantity(1);
    requisitionLineItem.setTotalStockoutDays(0);
    requisitionLineItem.setTotal(0);
    requisitionLineItem.setRequisition(requisition);

    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(requisitionLineItem);
    List<Comment> comments = new ArrayList<>();

    requisition.setRequisitionLineItems(requisitionLineItems);
    requisition.setComments(comments);
    requisition = requisitionRepository.save(requisition);

    RequisitionTemplate template = new RequisitionTemplate();
    template.setColumnsMap(generateTemplateColumns());
    template.setProgramId(program.getId());

    requisitionTemplateRepository.save(template);
  }

  @Test
  public void shouldFindRequisitions() {
    RequisitionDto[] response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam(PROGRAM, PROGRAM_UUID)
        .queryParam("processingPeriod", PERIOD_UUID)
        .queryParam(FACILITY, FACILITY_UUID)
        .queryParam("supervisoryNode", supervisoryNode.getId())
        .queryParam("requisitionStatus", RequisitionStatus.INITIATED)
        .queryParam("createdDateFrom", localDateTime.minusDays(2).toString())
        .queryParam("createdDateTo", localDateTime.plusDays(2).toString())
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionDto[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertEquals(1, response.length);
    for (RequisitionDto receivedRequisition : response) {
      assertEquals(
          receivedRequisition.getProgram().getId(),
          PROGRAM_UUID);
      assertEquals(
          receivedRequisition.getProcessingPeriod().getId(),
          PERIOD_UUID);
      assertEquals(
          receivedRequisition.getFacility().getId(),
          FACILITY_UUID);
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

    RequisitionDto response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .post(SUBMIT_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionDto.class);
    assertNotNull(response.getId());

    assertEquals(requisition.getId(), response.getId());
    assertEquals(RequisitionStatus.SUBMITTED, response.getStatus());
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
    requisition.setSupervisoryNodeId(supervisoryNode.getId());
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

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
  }

  @Test
  public void shouldInsertComment() {

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    createComment(user, requisition, "Previous comment");
    Comment userPostComment = new Comment(requisition);
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

    Comment comment = new Comment(requisition);
    comment.setBody(COMMENT_TEXT);
    comment.setAuthorId(user.getId());

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

  @Test
  public void shouldNotInitializeRequisitionWithIncorrectSuggestedPeriodId() {

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam(PROGRAM, program.getId())
        .queryParam(FACILITY, facility.getId())
        .queryParam("suggestedPeriod", UUID.randomUUID())
        .queryParam("emergency", false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldInitializeRequisition() {

    requisitionRepository.delete(requisition);
    requisitionRepository.delete(requisitionForSearch);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam(PROGRAM, program.getId())
        .queryParam(FACILITY, facility.getId())
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
        .pathParam("id", requisition.getId())
        .when()
        .post(AUTHORIZATION_URL)
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
        .pathParam("id", requisition.getId())
        .when()
        .post(AUTHORIZATION_URL)
        .then()
        .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenRequisition() {

    RequisitionDto response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionDto.class);

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
  public void shouldGetApprovedRequisitionsWithSortByAscendingFilterByAndPaging() {
    generateRequisitions(20);
    Integer pageSize = 10;
    String filterValue = "facilityNameA";

    RequisitionWithSupplyingDepotsDto[] response = restAssured.given()
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
        .extract().as(RequisitionWithSupplyingDepotsDto[].class);

    Assert.assertTrue(response.length <= pageSize);

    RequisitionDto previousRequisition = null;
    Set<UUID> userFacilities = fulfillmentFacilitiesReferenceDataService
        .getFulfillmentFacilities(user.getId())
        .stream().map(FacilityDto::getId).collect(Collectors.toSet());

    for (RequisitionWithSupplyingDepotsDto dto : response) {
      RequisitionDto requisition = dto.getRequisition();
      Assert.assertTrue(requisition.getStatus().equals(RequisitionStatus.APPROVED));

      String facilityName = requisition.getFacility().getName();
      Assert.assertTrue(facilityName.contains(filterValue));

      List<FacilityDto> facilities = dto.getSupplyingDepots();
      for (FacilityDto facility : facilities) {
        Assert.assertTrue(userFacilities.contains(facility.getId()));
      }

      if (previousRequisition != null) {
        ProgramDto program1 = previousRequisition.getProgram();
        ProgramDto program2 = requisition.getProgram();

        Assert.assertTrue(program1.getName().compareTo(program2.getName()) <= 0);
      }

      previousRequisition = requisition;
    }

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetApprovedRequisitionsWithSortByDescendingFilterByAndPaging() {
    generateRequisitions(30);
    Integer pageSize = 20;

    RequisitionWithSupplyingDepotsDto[] response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam("filterValue", FACILITY)
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
        .extract().as(RequisitionWithSupplyingDepotsDto[].class);

    Assert.assertTrue(response.length <= pageSize);

    RequisitionDto previousRequisition = null;
    Set<UUID> userFacilities = fulfillmentFacilitiesReferenceDataService
        .getFulfillmentFacilities(user.getId())
        .stream().map(FacilityDto::getId).collect(Collectors.toSet());

    for (RequisitionWithSupplyingDepotsDto dto : response) {
      RequisitionDto requisition = dto.getRequisition();
      Assert.assertTrue(requisition.getStatus().equals(RequisitionStatus.APPROVED));

      String facilityCode = requisition.getFacility().getCode();
      Assert.assertTrue(facilityCode.contains(FACILITY));

      List<FacilityDto> facilities = dto.getSupplyingDepots();
      for (FacilityDto facility : facilities) {
        Assert.assertTrue(userFacilities.contains(facility.getId()));
      }

      if (previousRequisition != null) {
        ProgramDto program1 = previousRequisition.getProgram();
        ProgramDto program2 = requisition.getProgram();

        Assert.assertTrue(program1.getName().compareTo(program2.getName()) >= 0);
      }

      previousRequisition = requisition;
    }

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetApprovedRequisitionsIfUserHasNoFulfillmentRightsForFacility() {
    // given
    generateRequisitions(5);
    final String fulfillmentFacilitiesResult = "[]";

    wireMockRule.stubFor(
        get(urlMatching("/referencedata/api/users/" + UUID_REGEX + "/fulfillmentFacilities.*"))
            .willReturn(aResponse()
                .withHeader(CONTENT_TYPE, APPLICATION_JSON)
                .withBody(fulfillmentFacilitiesResult)));

    // when
    RequisitionWithSupplyingDepotsDto[] response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionWithSupplyingDepotsDto[].class);

    // then
    assertEquals(response.length, 0);
  }

  @Test
  public void shouldGetApprovedRequisitionsWithUserFulfillmentRights() {
    // given
    int requisitionsAmount = 5;
    generateRequisitions(requisitionsAmount);

    wireMockRule.stubFor(get(
        urlMatching("/referencedata/api/facilities/supplying.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody("[]")));

    for (Requisition requisition : requisitionRepository.findAll()) {
      if (requisition.getFacilityId().toString().equals(FACILITY_ID)) {
        ValueMatchingStrategy strategy = new ValueMatchingStrategy();
        strategy.setMatches(requisition.getProgramId().toString());

        // This mocks searching for supplying facilities
        wireMockRule.stubFor(get(
            urlMatching("/referencedata/api/facilities/supplying.*"))
            .withQueryParam("programId", strategy)
            .willReturn(aResponse()
                .withHeader(CONTENT_TYPE, APPLICATION_JSON)
                .withBody(MOCK_SEARCH_SUPPLYING_FACILITY_RESULT)));
      }
    }

    // when
    RequisitionWithSupplyingDepotsDto[] response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionWithSupplyingDepotsDto[].class);

    // then
    assertEquals(requisitionsAmount, response.length);

    for (RequisitionWithSupplyingDepotsDto dto : response) {
      Assert.assertTrue(dto.getRequisition().getStatus().equals(RequisitionStatus.APPROVED));
    }
  }

  @Test
  public void shouldNotInitiateIfUserHasNoRight() throws Exception {
    denyUserAllRights();

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam(PROGRAM, program.getId())
        .queryParam(FACILITY, facility.getId())
        .queryParam("suggestedPeriod", UUID.randomUUID())
        .queryParam("emergency", false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(403);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitIfUserHasNoRight() throws Exception {
    denyUserAllRights();

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .post(SUBMIT_URL)
        .then()
        .statusCode(403);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteIfUserHasNoRight() throws Exception {
    denyUserAllRights();

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(403);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotRetrieveIfUserHasNoRight() throws Exception {
    denyUserAllRights();

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(403);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotApproveIfUserHasNoRight() throws Exception {
    denyUserAllRights();

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", requisition.getId())
        .when()
        .post(APPROVE_URL)
        .then()
        .statusCode(403);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotAuthorizeIfUserHasNoRight() throws Exception {
    denyUserAllRights();

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", requisition.getId())
        .when()
        .post(AUTHORIZATION_URL)
        .then()
        .statusCode(403);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldConvertRequisitionToOrder() {
    Requisition requisition = new Requisition();
    requisition.setProgramId(program.getId());
    requisition.setFacilityId(facility.getId());
    requisition.setProcessingPeriodId(period.getId());
    requisition.setStatus(RequisitionStatus.APPROVED);
    requisition.setEmergency(false);
    requisition.setSupervisoryNodeId(supervisoryNode.getId());

    wireMockRule.stubFor(
        post(urlMatching("/fulfillment/api/orders.*"))
        .willReturn(aResponse().withStatus(200)));

    requisitionRepository.save(requisition);

    UUID supplyingFacility = UUID.fromString("1d5bdd9c-8702-11e6-ae22-56b6b6499611");
    ConvertToOrderDto convertToOrderDto = new ConvertToOrderDto(
        requisition.getId(), supplyingFacility
    );

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(Collections.singletonList(convertToOrderDto))
        .when()
        .post("/api/requisitions/convertToOrder")
        .then()
        .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotConvertRequisitionToOrderIfSupplyingDepotsNotProvided() {
    ConvertToOrderDto convertToOrderDto =
        new ConvertToOrderDto(requisition.getId(), null);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(Collections.singletonList(convertToOrderDto))
        .when()
        .post("/api/requisitions/convertToOrder")
        .then()
        .statusCode(400);
  }

  @Test
  public void shouldNotConvertRequisitionToOrderIfUserHasNoFulfillmentRightsForFacility() {
    final String fulfillmentFacilitiesResult = "[]";
    UUID supplyingFacility = UUID.fromString("1d5bdd9c-8702-11e6-ae22-56b6b6499611");

    wireMockRule.stubFor(
        get(urlMatching("/referencedata/api/users/" + UUID_REGEX + "/fulfillmentFacilities.*"))
            .willReturn(aResponse()
                .withHeader(CONTENT_TYPE, APPLICATION_JSON)
                .withBody(fulfillmentFacilitiesResult)));

    ConvertToOrderDto convertToOrderDto =
        new ConvertToOrderDto(requisition.getId(), supplyingFacility);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(Collections.singletonList(convertToOrderDto))
        .when()
        .post("/api/requisitions/convertToOrder")
        .then()
        .statusCode(400);
  }

  private void testApproveRequisition(Requisition requisition) {

    Requisition response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", requisition.getId())
        .when()
        .post(APPROVE_URL)
        .then()
        .statusCode(200)
        .extract().as(Requisition.class);

    assertNotNull(response.getId());
    assertEquals(requisition.getId(), response.getId());
    assertEquals(RequisitionStatus.APPROVED, response.getStatus());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private Requisition configureRequisition(Requisition requisition) {
    requisition.setFacilityId(facility.getId());
    requisition.setProcessingPeriodId(period.getId());
    requisition.setProgramId(program.getId());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setSupervisoryNodeId(supervisoryNode.getId());
    requisition.setCreatedDate(localDateTime);
    requisition.setEmergency(false);

    return requisitionRepository.save(requisition);
  }

  private Requisition configureRequisitionForSearch(Requisition requisition) {
    requisition.setFacilityId(FACILITY_UUID);
    requisition.setProcessingPeriodId(PERIOD_UUID);
    requisition.setProgramId(PROGRAM_UUID);
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setSupervisoryNodeId(supervisoryNode.getId());
    requisition.setCreatedDate(localDateTime);
    requisition.setEmergency(false);

    return requisitionRepository.save(requisition);
  }

  private Comment createComment(UserDto author, Requisition req, String commentText) {
    Comment comment = new Comment(req);
    comment.setAuthorId(author.getId());
    comment.setBody(commentText);
    commentRepository.save(comment);
    return comment;
  }

  private Requisition generateRequisition(RequisitionStatus requisitionStatus, UUID facility) {
    Requisition requisition = new Requisition(facility, UUID.randomUUID(), UUID.randomUUID(),
        requisitionStatus, true);
    requisition.setId(UUID.randomUUID());
    requisition.setCreatedDate(LocalDateTime.now());
    requisitionRepository.save(requisition);

    return requisition;
  }

  private void generateRequisitions(int amount) {
    for (int i = 0; i < amount; i++) {
      generateRequisition(RequisitionStatus.APPROVED, UUID.fromString(FACILITY_ID));
      generateRequisition(RequisitionStatus.APPROVED, getSharedFacilityId());
      generateRequisition(RequisitionStatus.SUBMITTED, UUID.randomUUID());
    }
  }

  private Map<String, RequisitionTemplateColumn> generateTemplateColumns() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();

    for (AvailableRequisitionColumn columnDefinition :
        availableRequisitionColumnRepository.findAll()) {
      RequisitionTemplateColumn column = new RequisitionTemplateColumn(columnDefinition);
      column.setName(columnDefinition.getName());
      column.setIsDisplayed(true);
      columns.put(columnDefinition.getName(), column);
    }
    return columns;
  }

  private void denyUserAllRights() {
    wireMockRule.stubFor(
        get(urlMatching(REFERENCEDATA_API_USERS + UUID_REGEX + "/hasRight.*"))
            .willReturn(aResponse()
                .withHeader(CONTENT_TYPE, APPLICATION_JSON)
                .withBody("{ \"result\":\"false\" }"))
    );
  }
}
