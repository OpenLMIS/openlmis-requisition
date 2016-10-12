package org.openlmis.requisition.web;

import guru.nidi.ramltester.junit.RamlMatchers;
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

import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

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
  private static final String FACILITY = "facility";
  private static final String APPROVED_REQUISITIONS_SEARCH_URL =
      RESOURCE_URL + "/requisitions-for-convert";

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
  private ProcessingPeriodDto period = new ProcessingPeriodDto();
  private OrderableProductDto product = new OrderableProductDto();
  private ProgramDto program = new ProgramDto();
  private ProgramDto supervisedProgram = new ProgramDto();
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

    supervisedProgram.setId(UUID.fromString("5c5a6f68-8658-11e6-ae22-56b6b6499611"));
    supervisedProgram.setCode(REQUISITION_REPOSITORY_NAME);

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

    requisitionLineItem.setOrderableProductId(product.getId());
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setRequestedQuantityExplanation("Requested Quantity Explanation");
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

    RequisitionTemplate template = new RequisitionTemplate();
    template.setColumnsMap(generateTemplateColumns());
    template.setProgramId(program.getId());
    requisitionTemplateRepository.save(template);

    template = new RequisitionTemplate();
    template.setColumnsMap(generateTemplateColumns());
    template.setProgramId(supervisedProgram.getId());
    requisitionTemplateRepository.save(template);
  }


  @Test
  public void shouldFindRequisitions() {
    Requisition[] response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam("program", program.getId())
        .queryParam("processingPeriod", period.getId())
        .queryParam(FACILITY, facility.getId())
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
              receivedRequisition.getProgramId(),
              program.getId());
      assertEquals(
              receivedRequisition.getProcessingPeriodId(),
              period.getId());
      assertEquals(
              receivedRequisition.getFacilityId(),
              facility.getId());
      assertEquals(
              receivedRequisition.getSupervisoryNodeId(),
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
    requisitionLineItem.setOrderableProductId(product.getId());
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
        + "\"requestedQuantity must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullBeginningBalanceInRequisitionLineItem() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setRequestedQuantityExplanation("Requested Quantity Explanation");
    requisitionLineItem.setOrderableProductId(product.getId());
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
        + "\"beginningBalance must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNegativeBeginningBalanceInRequisitionLineItem() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setRequestedQuantityExplanation("Quantity Explanation");
    requisitionLineItem.setBeginningBalance(-1);
    requisitionLineItem.setOrderableProductId(product.getId());
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
        + "\"beginningBalance must be a non-negative value.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullTotalReceivedQuantityInRequisitionLineItem() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setRequestedQuantityExplanation("Quantity Explanation");
    requisitionLineItem.setOrderableProductId(product.getId());
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
        + "\"totalReceivedQuantity must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNegativeTotalReceivedQuantityInRequisitionLineItem() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setRequestedQuantityExplanation("Quantity Explanation");
    requisitionLineItem.setBeginningBalance(1);
    requisitionLineItem.setOrderableProductId(product.getId());
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
        + "\"totalReceivedQuantity must be a non-negative value.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullStockHandInRequisitionLineItem() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setRequestedQuantityExplanation("Explanation");
    requisitionLineItem.setBeginningBalance(1);
    requisitionLineItem.setOrderableProductId(product.getId());
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
        + "\"stockOnHand must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullConsumedQuantityInRequisitionLineItem() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setRequestedQuantityExplanation("Explanation");
    requisitionLineItem.setBeginningBalance(1);
    requisitionLineItem.setOrderableProductId(product.getId());
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
        + "\"totalConsumedQuantity must be entered prior to submission of a requisition.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWhenRequestedQuantitySetWitnNoExplanation() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setBeginningBalance(1);
    requisitionLineItem.setOrderableProductId(product.getId());
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
        + "\"requestedQuantityExplanation must be entered"
        + " when requested quantity is not empty.\"\n}";

    assertTrue(response.contains(expectedExceptionMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitRequisitionWithNullAttributesInRequisitionLineItem() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setOrderableProductId(product.getId());
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
  public void
      shouldNotSubmitRequisitionWhenTotalConsumedQuantityCalculatedAndAtLeastOneLineIsNotFilled() {

    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setOrderableProductId(product.getId());
    requisitionLineItem.setStockOnHand(null);
    requisitionLineItem.setTotalConsumedQuantity(1);
    requisitionLineItem.setBeginningBalance(1);
    requisitionLineItem.setTotalReceivedQuantity(1);
    requisitionLineItem.setTotalLossesAndAdjustments(1);

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

  @Test
  public void shouldInitializeRequisition() {

    requisitionRepository.delete(requisition);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam("program", supervisedProgram.getId())
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
  public void shouldNotInitializeIfUserIsNotAssociatedWithGivenProgram() {

    requisitionRepository.delete(requisition);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam("program", program.getId())
        .queryParam(FACILITY, facility.getId())
        .queryParam("suggestedPeriod", period.getId())
        .queryParam("emergency", false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(400);

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
    requisition.setFacilityId(facility.getId());
    requisition.setProcessingPeriodId(period.getId());
    requisition.setProgramId(program.getId());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setSupervisoryNodeId(supervisoryNode.getId());
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
    requisition.setFacilityId(facility);
    requisition.setProcessingPeriodId(UUID.randomUUID());
    requisition.setProgramId(UUID.randomUUID());
    requisition.setCreatedDate(LocalDateTime.now());
    requisition.setStatus(requisitionStatus);
    requisition.setEmergency(true);
    requisitionRepository.save(requisition);
  }

  private void generateRequisitions() {
    for (int i = 0; i < 4; i++) {
      UUID facility1 = getSharedFacilityId();
      UUID facility2 = UUID.randomUUID();
      generateRequisition(RequisitionStatus.APPROVED, facility2);
      generateRequisition(RequisitionStatus.SUBMITTED, facility1);
      for (int j = 0; j < 4; j++) {
        generateRequisition(RequisitionStatus.APPROVED, facility1);
      }
    }
  }


  private Map<String, RequisitionTemplateColumn> generateTemplateColumns() {
    Map<String, RequisitionTemplateColumn> columns = new HashMap<>();

    for (AvailableRequisitionColumn columnDefinition :
        availableRequisitionColumnRepository.findAll()) {
      RequisitionTemplateColumn column = new RequisitionTemplateColumn();
      column.setColumnDefinition(columnDefinition);
      column.setName(columnDefinition.getName());
      column.setIsDisplayed(true);
      columns.put(columnDefinition.getName(), column);
    }
    return columns;
  }

  @Test
  public void shouldGetApprovedRequisitionsWithSortByAscendingFilterByAndPaging() {
    generateRequisitions();
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
    generateRequisitions();
    Integer pageSize = 20;
    String filterValue = FACILITY;

    RequisitionWithSupplyingDepotsDto[] response = restAssured.given()
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
      Assert.assertTrue(facilityCode.contains(filterValue));

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
}
