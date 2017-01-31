package org.openlmis.requisition.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.repository.AvailableRequisitionColumnRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import guru.nidi.ramltester.junit.RamlMatchers;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionTemplateControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/requisitionTemplates";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String PROGRAM = "program";
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");

  private static final String REQUESTED_QUANTITY = "requestedQuantity";
  private static final String REQUESTED_QUANTITY_EXPLANATION = "requestedQuantityExplanation";
  private static final String TOTAL_CONSUMED_QUANTITY = "totalConsumedQuantity";
  private static final String BEGINNING_BALANCE = "beginningBalance";
  private static final String TOTAL_RECEIVED_QUANTITY = "totalReceivedQuantity";
  private static final String TOTAL_LOSSES_AND_ADJUSTMENTS = "totalLossesAndAdjustments";
  private static final String STOCK_ON_HAND = "stockOnHand";
  private static final String ADJUSTED_CONSUMPTION = "adjustedConsumption";
  private static final String TOTAL_STOCKOUT_DAYS = "totalStockoutDays";

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private AvailableRequisitionColumnRepository availableRequisitionColumnRepository;

  private RequisitionTemplate requisitionTemplate;
  private Integer currentInstanceNumber;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    requisitionTemplate = generateRequisitionTemplate();
  }

  @After
  public void cleanUp() {
    requisitionRepository.deleteAll();
  }

  @Test
  public void shouldFindRequisitionTemplates() {
    RequisitionTemplate response = restAssured.given()
        .queryParam(PROGRAM, requisitionTemplate.getProgramId())
        .queryParam(ACCESS_TOKEN, getToken())
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionTemplate.class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());

    assertNotNull(response);
    assertEquals(
        requisitionTemplate.getProgramId(),
        response.getProgramId());
    assertEquals(
        requisitionTemplate.getId(),
        response.getId());
  }

  @Test
  public void shouldDeleteRequisitionTemplate() {

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionTemplate.getId())
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(204);

    assertFalse(requisitionTemplateRepository.exists(requisitionTemplate.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }


  @Test
  public void shouldReturnBadRequestWhenNotAllowedToDeleteRequisition() {
    generateRequisition();
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionTemplate.getId())
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(400);

    assertTrue(requisitionTemplateRepository.exists(requisitionTemplate.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteNonexistentRequisitionTemplate() {

    requisitionTemplateRepository.delete(requisitionTemplate);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionTemplate.getId())
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateRequisitionTemplate() {

    requisitionTemplateRepository.delete(requisitionTemplate);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(requisitionTemplate)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateRequisitionTemplate() {

    ProgramDto program = generateProgram();
    requisitionTemplate.setProgramId(program.getId());

    RequisitionTemplate response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionTemplate.getId())
        .body(requisitionTemplate)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionTemplate.class);

    assertEquals(response.getProgramId(), program.getId());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewRequisitionTemplateIfDoesNotExist() {

    requisitionTemplateRepository.delete(requisitionTemplate);
    ProgramDto program = generateProgram();
    requisitionTemplate.setProgramId(program.getId());

    RequisitionTemplate response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", ID)
        .body(requisitionTemplate)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionTemplate.class);

    assertEquals(response.getProgramId(), program.getId());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllRequisitionTemplates() {

    RequisitionTemplate[] response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(RESOURCE_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionTemplate[].class);

    Iterable<RequisitionTemplate> requisitionTemplates = Arrays.asList(response);
    assertTrue(requisitionTemplates.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenRequisitionTemplate() {

    RequisitionTemplate response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionTemplate.getId())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionTemplate.class);

    assertTrue(requisitionTemplateRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonexistentRequisitionTemplate() {

    requisitionTemplateRepository.delete(requisitionTemplate);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionTemplate.getId())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSaveWhenRequestedQuantityAndExplanationDisplayedValuesAreDifferent() {
    requisitionTemplate.changeColumnDisplay(REQUESTED_QUANTITY, false);

    String response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionTemplate.getId())
        .body(requisitionTemplate)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(400)
        .extract().asString();

    String expectedMessage = REQUESTED_QUANTITY
        + " must be displayed when requested quantity explanation is displayed.";

    assertTrue(response.contains(expectedMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSaveWhenTotalConsumedQuantityCalculatedAndStockOnHandIsNotDisplayed() {
    requisitionTemplate.changeColumnDisplay(STOCK_ON_HAND, false);
    requisitionTemplate.changeColumnSource(TOTAL_CONSUMED_QUANTITY, SourceType.CALCULATED);

    String response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionTemplate.getId())
        .body(requisitionTemplate)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(400)
        .extract().asString();

    String expectedMessage = STOCK_ON_HAND
        + " must be displayed";

    assertTrue(response.contains(expectedMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSaveWhenAdjustedConsumptionIsOnTemplateAndStockoutDaysNotDisplayed() {
    requisitionTemplate.changeColumnDisplay(TOTAL_STOCKOUT_DAYS, false);

    String response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionTemplate.getId())
        .body(requisitionTemplate)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(400)
        .extract().asString();

    String expectedMessage = TOTAL_STOCKOUT_DAYS
        + " must be displayed when adjusted consumption is calculated.";

    assertTrue(response.contains(expectedMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSaveWhenAdjustedConsumptionIsOnTemplateAndtConsumedQuantityNotDisplayed() {
    requisitionTemplate.changeColumnDisplay(TOTAL_CONSUMED_QUANTITY, false);

    String response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisitionTemplate.getId())
        .body(requisitionTemplate)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(400)
        .extract().asString();

    String expectedMessage = TOTAL_CONSUMED_QUANTITY
        + " must be displayed";

    assertTrue(response.contains(expectedMessage));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private RequisitionTemplate generateRequisitionTemplate() {
    AvailableRequisitionColumn columnDefinition = new AvailableRequisitionColumn();
    columnDefinition.setIsDisplayRequired(false);

    RequisitionTemplateColumn column = new RequisitionTemplateColumn(columnDefinition);
    column.setName(REQUESTED_QUANTITY);
    column.setLabel("Requested Quantity");
    column.setIsDisplayed(true);
    column.setColumnDefinition(availableRequisitionColumnRepository.findOne(
        UUID.fromString("4a2e9fd3-1127-4b68-9912-84a5c00f6999")
    ));
    Map<String, RequisitionTemplateColumn> columnMap = new HashMap<>();
    columnMap.put(REQUESTED_QUANTITY, column);

    column = new RequisitionTemplateColumn(columnDefinition);
    column.setName(REQUESTED_QUANTITY_EXPLANATION);
    column.setLabel("Requested Quantity Explanation");
    column.setIsDisplayed(true);
    column.setColumnDefinition(availableRequisitionColumnRepository.findOne(
        UUID.fromString("6b8d331b-a0dd-4a1f-aafb-40e6a72ab9f5")
    ));
    columnMap.put(REQUESTED_QUANTITY_EXPLANATION, column);

    column = new RequisitionTemplateColumn(columnDefinition);
    column.setName(TOTAL_CONSUMED_QUANTITY);
    column.setLabel("Total Consumed Quantity");
    column.setIsDisplayed(true);
    column.setColumnDefinition(availableRequisitionColumnRepository.findOne(
        UUID.fromString("9e825396-269d-4873-baa4-89054e2722f4")
    ));
    columnMap.put(TOTAL_CONSUMED_QUANTITY, column);

    column = new RequisitionTemplateColumn(columnDefinition);
    column.setName(BEGINNING_BALANCE);
    column.setLabel("Beginning Balance");
    column.setIsDisplayed(true);
    column.setColumnDefinition(availableRequisitionColumnRepository.findOne(
        UUID.fromString("33b2d2e9-3167-46b0-95d4-1295be9afc22")
    ));
    columnMap.put(BEGINNING_BALANCE, column);

    column = new RequisitionTemplateColumn(columnDefinition);
    column.setName(TOTAL_RECEIVED_QUANTITY);
    column.setLabel("Total Received Quantity");
    column.setIsDisplayed(true);
    column.setColumnDefinition(availableRequisitionColumnRepository.findOne(
        UUID.fromString("5ba8b72d-277a-4da8-b10a-23f0cda23cb4")
    ));
    columnMap.put(TOTAL_RECEIVED_QUANTITY, column);

    column = new RequisitionTemplateColumn(columnDefinition);
    column.setName(TOTAL_LOSSES_AND_ADJUSTMENTS);
    column.setLabel("Total Losses And Adjustments");
    column.setIsDisplayed(true);
    column.setColumnDefinition(availableRequisitionColumnRepository.findOne(
        UUID.fromString("cd57f329-f549-4717-882e-ecbf98122c38")
    ));
    columnMap.put(TOTAL_LOSSES_AND_ADJUSTMENTS, column);

    column = new RequisitionTemplateColumn(columnDefinition);
    column.setName(STOCK_ON_HAND);
    column.setLabel("Stock On Hand");
    column.setIsDisplayed(true);
    column.setColumnDefinition(availableRequisitionColumnRepository.findOne(
        UUID.fromString("752cda76-0db5-4b6e-bb79-0f531ab78e2c")
    ));
    columnMap.put(STOCK_ON_HAND, column);

    column = new RequisitionTemplateColumn(columnDefinition);
    column.setName(TOTAL_STOCKOUT_DAYS);
    column.setLabel("Total Stockout Days");
    column.setIsDisplayed(true);
    column.setColumnDefinition(availableRequisitionColumnRepository.findOne(
        UUID.fromString("750b9359-c097-4612-8328-d21671f88920")
    ));
    columnMap.put(TOTAL_STOCKOUT_DAYS, column);

    column = new RequisitionTemplateColumn(columnDefinition);
    column.setName(ADJUSTED_CONSUMPTION);
    column.setLabel("Adjusted Consumption");
    column.setIsDisplayed(true);
    column.setColumnDefinition(availableRequisitionColumnRepository.findOne(
        UUID.fromString("720dd95b-b765-4afb-b7f2-7b22261c32f3")
    ));
    columnMap.put(ADJUSTED_CONSUMPTION, column);

    RequisitionTemplate reqTemplate = new RequisitionTemplate(columnMap);
    reqTemplate.changeColumnSource(REQUESTED_QUANTITY, SourceType.USER_INPUT);
    reqTemplate.changeColumnSource(STOCK_ON_HAND, SourceType.USER_INPUT);
    reqTemplate.changeColumnSource(REQUESTED_QUANTITY_EXPLANATION, SourceType.USER_INPUT);
    reqTemplate.changeColumnSource(TOTAL_CONSUMED_QUANTITY, SourceType.USER_INPUT);
    reqTemplate.changeColumnSource(BEGINNING_BALANCE, SourceType.USER_INPUT);
    reqTemplate.changeColumnSource(TOTAL_RECEIVED_QUANTITY, SourceType.USER_INPUT);
    reqTemplate.changeColumnSource(TOTAL_LOSSES_AND_ADJUSTMENTS, SourceType.USER_INPUT);
    reqTemplate.changeColumnSource(TOTAL_STOCKOUT_DAYS, SourceType.USER_INPUT);
    reqTemplate.changeColumnSource(ADJUSTED_CONSUMPTION, SourceType.CALCULATED);
    reqTemplate.setProgramId(generateProgram().getId());
    requisitionTemplateRepository.save(reqTemplate);
    return reqTemplate;
  }

  private ProgramDto generateProgram() {
    ProgramDto program = new ProgramDto();
    program.setId(UUID.randomUUID());
    program.setCode("code" + generateInstanceNumber());
    program.setPeriodsSkippable(false);
    return program;
  }

  private Requisition generateRequisition() {
    Requisition requisition = new Requisition(UUID.randomUUID(), UUID.randomUUID(),
        UUID.randomUUID(), UUID.randomUUID(), RequisitionStatus.INITIATED, true);

    requisition.setId(UUID.randomUUID());
    requisition.setCreatorId(UUID.randomUUID());
    requisition.setCreatedDate(LocalDateTime.now());
    requisition.setTemplate(requisitionTemplate);
    requisition.setNumberOfMonthsInPeriod(1);
    return requisitionRepository.save(requisition);
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }
}
