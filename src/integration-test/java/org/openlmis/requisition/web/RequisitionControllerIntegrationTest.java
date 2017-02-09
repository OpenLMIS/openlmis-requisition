package org.openlmis.requisition.web;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_CONVERT_TO_ORDER_CONTENT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_CONVERT_TO_ORDER_SUBJECT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_NOREPLY;
import static org.openlmis.utils.FacilitySupportsProgramHelper.REQUISITION_TIME_ZONE_ID;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.util.CollectionUtils.isEmpty;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.client.ValueMatchingStrategy;

import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.CurrencyConfig;
import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.framework.PageImplRepresentation;
import org.openlmis.requisition.i18n.ExposedMessageSource;
import org.openlmis.requisition.repository.AvailableRequisitionColumnRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.settings.domain.ConfigurationSetting;
import org.openlmis.settings.repository.ConfigurationSettingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.http.MediaType;

import guru.nidi.ramltester.junit.RamlMatchers;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.ZonedDateTime;
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

  private static final String REQUISITION_REPOSITORY_NAME = "RequisitionRepositoryIntegrationTest";
  private static final String RESOURCE_URL = "/api/requisitions";
  private static final String INITIATE_URL = RESOURCE_URL + "/initiate";
  private static final String APPROVE_URL = RESOURCE_URL + "/{id}/approve";
  private static final String SKIP_URL = RESOURCE_URL + "/{id}/skip";
  private static final String REJECT_URL = RESOURCE_URL + "/{id}/reject";
  private static final String SUBMIT_URL = RESOURCE_URL + "/{id}/submit";
  private static final String SUBMITTED_URL = RESOURCE_URL + "/submitted";
  private static final String AUTHORIZATION_URL = RESOURCE_URL + "/{id}/authorize";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String REQ_FOR_APPROVAL_URL = RESOURCE_URL + "/requisitionsForApproval";
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");
  private static final String FACILITY = "facility";
  private static final String APPROVED_REQUISITIONS_SEARCH_URL =
      RESOURCE_URL + "/requisitionsForConvert";
  private static final UUID PERIOD_UUID = UUID.fromString("4c6b05c2-894b-11e6-ae22-56b6b6499611");
  private static final UUID PROGRAM_UUID = UUID.fromString("5c5a6f68-8658-11e6-ae22-56b6b6499611");
  private static final UUID FACILITY_UUID = UUID.fromString("1d5bdd9c-8702-11e6-ae22-56b6b6499611");
  private static final String PROGRAM = "program";
  private static final String SUGGESTED_PERIOD = "suggestedPeriod";
  private static final String EMERGENCY = "emergency";
  private static final String MESSAGE = "message";
  private static final String FACILITY_CODE = "facilityCode";
  private static final String SUPERVISORY_SEARCH_URL = "/api/supervisoryNodes/search";
  private static final String SUPERVISORY_URL = "/api/supervisoryNodes/";

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private ConfigurationSettingRepository configurationSettingRepository;

  @Autowired
  private UserFulfillmentFacilitiesReferenceDataService fulfillmentFacilitiesReferenceDataService;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private AvailableRequisitionColumnRepository availableRequisitionColumnRepository;

  @Autowired
  private ExposedMessageSource messageSource;

  private RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
  private Requisition requisition = new Requisition();
  private Requisition requisitionForSearch = new Requisition();
  private ProcessingPeriodDto period = new ProcessingPeriodDto();
  private OrderableDto ordereble = new OrderableDto();
  private ProgramDto programDto = new ProgramDto();
  private FacilityDto facilityDto = new FacilityDto();
  private SupervisoryNodeDto supervisoryNode = new SupervisoryNodeDto();
  private SupervisoryNodeDto parentSupervisoryNode = new SupervisoryNodeDto();
  private UserDto user;
  private ZonedDateTime createdDate = ZonedDateTime.now();
  private RequisitionTemplate template;

  @Before
  public void setUp() throws IOException {
    user = new UserDto();
    user.setId(INITIAL_USER_ID);
    user.setUsername("admin");
    user.setFirstName("Admin");
    user.setLastName("User");
    user.setEmail("example@mail.com");

    ordereble.setId(UUID.fromString("cd9e1412-8703-11e6-ae22-56b6b6499611"));

    programDto.setId(UUID.fromString("86191d25-4846-4775-a968-12df732e6004"));
    programDto.setCode(REQUISITION_REPOSITORY_NAME);
    programDto.setPeriodsSkippable(true);

    facilityDto.setId(getSharedFacilityId());
    facilityDto.setCode(FACILITY_CODE);
    facilityDto.setActive(true);
    facilityDto.setEnabled(true);

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
    supervisoryNode.setFacility(facilityDto);
    parentSupervisoryNode.setId(UUID.randomUUID());
    supervisoryNode.setParentNode(parentSupervisoryNode);

    template = new RequisitionTemplate();
    template.setColumnsMap(generateTemplateColumns());
    template.setProgramId(programDto.getId());
    template.setNumberOfPeriodsToAverage(5);

    requisitionTemplateRepository.save(template);

    configureRequisition(requisition);
    requisitionForSearch.setStatus(RequisitionStatus.INITIATED);
    configureRequisitionForSearch(requisitionForSearch);

    requisitionLineItem.setOrderableId(ordereble.getId());
    requisitionLineItem.setMaxPeriodsOfStock(BigDecimal.valueOf(2));
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
    requisitionLineItem.setNumberOfNewPatientsAdded(0);
    requisitionLineItem.setAverageConsumption(2);
    requisitionLineItem.setMaximumStockQuantity(4);
    requisitionLineItem.setCalculatedOrderQuantity(2);
    requisitionLineItem.setRequisition(requisition);
    requisitionLineItem.setPricePerPack(
        Money.of(CurrencyUnit.of(CurrencyConfig.CURRENCY_CODE), 13.55));

    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(requisitionLineItem);

    requisition.setRequisitionLineItems(requisitionLineItems);
    requisition = requisitionRepository.save(requisition);

    configurationSettingRepository.save(new ConfigurationSetting(REQUISITION_TIME_ZONE_ID, "UTC"));
  }

  @Test
  public void shouldFindRequisitions() {
    PageImplRepresentation<RequisitionDto> response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam(PROGRAM, PROGRAM_UUID)
        .queryParam("processingPeriod", PERIOD_UUID)
        .queryParam(FACILITY, FACILITY_UUID)
        .queryParam("supervisoryNode", supervisoryNode.getId())
        .queryParam("requisitionStatus", RequisitionStatus.INITIATED)
        .queryParam("createdDateFrom", createdDate.minusDays(2).toString())
        .queryParam("createdDateTo", createdDate.plusDays(2).toString())
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    //Extract typed content from the PageImpl response
    ObjectMapper mapper = new ObjectMapper();
    mapper.findAndRegisterModules();
    List<RequisitionDto> content = mapper.convertValue(response.getContent(),
        new TypeReference<List<RequisitionDto>>() {
        });

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertEquals(1, content.size());
    for (RequisitionDto receivedRequisition : content) {
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
          receivedRequisition.getCreatedDate().isBefore(createdDate.plusDays(2)));
      assertTrue(
          receivedRequisition.getCreatedDate().isAfter(createdDate.minusDays(2)));
    }
  }

  @Test
  public void shouldFindRequisitionsWithStatuses() {
    Requisition req = new Requisition();
    req.setStatus(RequisitionStatus.SUBMITTED);
    configureRequisitionForSearch(req);

    PageImplRepresentation<RequisitionDto> response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam(PROGRAM, PROGRAM_UUID)
        .queryParam("processingPeriod", PERIOD_UUID)
        .queryParam(FACILITY, FACILITY_UUID)
        .queryParam("supervisoryNode", supervisoryNode.getId())
        .queryParam("requisitionStatus", RequisitionStatus.INITIATED)
        .queryParam("requisitionStatus", RequisitionStatus.SUBMITTED)
        .queryParam("createdDateFrom", createdDate.minusDays(2).toString())
        .queryParam("createdDateTo", createdDate.plusDays(2).toString())
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    //Extract typed content from the PageImpl response
    ObjectMapper mapper = new ObjectMapper();
    mapper.findAndRegisterModules();
    List<RequisitionDto> content = mapper.convertValue(response.getContent(),
        new TypeReference<List<RequisitionDto>>() {
        });

    assertEquals(2, content.size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
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
  public void shouldSerializeMoneyOnSubmit() {
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

    RequisitionLineItem.Importer importer = response.getRequisitionLineItems().get(0);
    assertEquals(new BigDecimal("13.55"), importer.getPricePerPack().getAmount());
    assertEquals(CurrencyUnit.of(CurrencyConfig.CURRENCY_CODE),
        importer.getPricePerPack().getCurrencyUnit());
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
  public void shouldNotSkipRequisitionIfUserHasNoRights() {
    denyUserAllRights();

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(SKIP_URL)
        .then()
        .statusCode(403);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSkipRequisitionIfItIsNotInitiated() {
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(SKIP_URL)
        .then()
        .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSkipRequisitionIfItIsEmergency() {
    requisition.setEmergency(true);
    requisitionRepository.save(requisition);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(SKIP_URL)
        .then()
        .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturnNotFoundWhenSkippingNotExistingRequisition() {
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", UUID.randomUUID())
        .when()
        .put(SKIP_URL)
        .then()
        .statusCode(404);

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
  public void shouldNotRejectRequisitionIfUserHasNoRights() {

    denyUserAllRights();
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .put(REJECT_URL)
        .then()
        .statusCode(403);

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

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
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
  public void shouldGetRequisitionsForApprovalForSpecificUser() {
    mockFacility();
    mockDetailedRoleAssignmentDto();
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    RequisitionDto[] response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(REQ_FOR_APPROVAL_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionDto[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    List<RequisitionDto> responseList = Arrays.asList(response);
    List<Requisition> expectedRequisitionList = Collections.singletonList(requisition);

    for (int i = 0; i < responseList.size(); i++) {
      assertEquals(expectedRequisitionList.get(i).getId(), responseList.get(i).getId());
    }
  }

  @Test
  public void shouldApproveSubmittedRequisitionIfSkippedAuthorization() {
    configurationSettingRepository.save(new ConfigurationSetting("skipAuthorization", "true"));
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);

    mockSupervisoryNode();
    RequisitionDto response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", requisition.getId())
        .when()
        .post(APPROVE_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionDto.class);

    assertEquals(requisition.getId(), response.getId());
    assertEquals(RequisitionStatus.APPROVED, response.getStatus());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotInitializeRequisitionWithIncorrectSuggestedPeriodId() {
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam(PROGRAM, programDto.getId())
        .queryParam(FACILITY, facilityDto.getId())
        .queryParam(SUGGESTED_PERIOD, UUID.randomUUID())
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldInitiateRequisition() {
    requisitionRepository.delete(requisition);
    requisitionRepository.delete(requisitionForSearch);

    RequisitionDto response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam(PROGRAM, programDto.getId())
        .queryParam(FACILITY, facilityDto.getId())
        .queryParam(SUGGESTED_PERIOD, period.getId())
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(201)
        .extract().as(RequisitionDto.class);

    assertEquals(user.getId(), response.getCreatorId());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetSubmittedRequisitions() {

    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);

    PageImplRepresentation<RequisitionDto> response = new PageImplRepresentation<>();
    response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(SUBMITTED_URL)
        .then()
        .statusCode(200)
        .extract().as(response.getClass());

    Iterable<RequisitionDto> requisitions = response.getContent();
    assertTrue(requisitions.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldAuthorizeRequisition() {
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);

    mockSupervisoryNodeSearch();
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", requisition.getId())
        .when()
        .post(AUTHORIZATION_URL)
        .then()
        .statusCode(200);

    requisition = requisitionRepository.findOne(requisition.getId());
    assertEquals(ID, requisition.getSupervisoryNodeId());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNullSkippedRequisitionLineItems() {

    RequisitionDto response = getRequisitionDtoForCheckNullingLineItemsValues();

    List<RequisitionLineItem.Importer> requisitionLineItemsRetrieved =
        response.getRequisitionLineItems();

    requisitionLineItemsRetrieved.stream()
        .filter(RequisitionLineItem.Importer::getSkipped)
        .forEach(line -> {
          assertEquals(null, line.getBeginningBalance());
          assertEquals(null, line.getTotalReceivedQuantity());
          assertEquals(null, line.getTotalLossesAndAdjustments());
          assertEquals(null, line.getStockOnHand());
          assertEquals(null, line.getRequestedQuantityExplanation());
          assertEquals(null, line.getRemarks());
          assertEquals(null, line.getApprovedQuantity());
          assertEquals(null, line.getRequestedQuantity());
          assertEquals(null, line.getTotalConsumedQuantity());
          assertEquals(null, line.getTotal());
          assertEquals(null, line.getRequestedQuantityExplanation());
          assertEquals(null, line.getTotalStockoutDays());
          assertEquals(null, line.getPacksToShip());
          assertEquals(null, line.getPricePerPack());
          assertEquals(null, line.getTotalCost());
          assertEquals(null, line.getNumberOfNewPatientsAdded());
          assertEquals(null, line.getAdjustedConsumption());
          assertEquals(null, line.getAverageConsumption());
          assertEquals(0, line.getStockAdjustments().size());
        });

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotNullNotSkippedRequisitionLineItems() {

    RequisitionDto response = getRequisitionDtoForCheckNullingLineItemsValues();

    List<RequisitionLineItem.Importer> requisitionLineItemsRetrieved =
        response.getRequisitionLineItems();

    requisitionLineItemsRetrieved.stream()
        .filter(line -> !line.getSkipped())
        .forEach(line -> {
          assertNotNull(line.getBeginningBalance());
          assertNotNull(line.getTotalReceivedQuantity());
          assertNotNull(line.getTotalLossesAndAdjustments());
          assertNotNull(line.getStockOnHand());
          assertNotNull(line.getRequestedQuantityExplanation());
          assertNotNull(line.getApprovedQuantity());
          assertNotNull(line.getRequestedQuantity());
          assertNotNull(line.getTotalConsumedQuantity());
          assertNotNull(line.getTotal());
          assertNotNull(line.getRequestedQuantityExplanation());
          assertNotNull(line.getTotalStockoutDays());
          assertNotNull(line.getPacksToShip());
          assertNotNull(line.getNumberOfNewPatientsAdded());
          assertNotNull(line.getAdjustedConsumption());
          assertNotNull(line.getAverageConsumption());
        });

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private RequisitionDto getRequisitionDtoForCheckNullingLineItemsValues() {
    requisition.setStatus(RequisitionStatus.SUBMITTED);

    requisitionLineItem.setSkipped(true);

    List<RequisitionLineItem> requisitionLineItems =
        requisition.getRequisitionLineItems();
    requisitionLineItems.add(requisitionLineItem);

    requisition.setRequisitionLineItems(requisitionLineItems);

    requisitionRepository.save(requisition);

    mockSupervisoryNodeSearch();
    return restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", requisition.getId())
        .when()
        .post(AUTHORIZATION_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionDto.class);
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
    int numberOfRequisitions = 20;
    int pageSize = 10;
    generateRequisitions(numberOfRequisitions);
    String filterValue = "facility NameA";

    PageImplRepresentation<RequisitionWithSupplyingDepotsDto> response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam("filterValue", filterValue)
        .queryParam("filterBy", "facilityName")
        .queryParam("sortBy", FACILITY_CODE)
        .queryParam("descending", Boolean.FALSE.toString())
        .queryParam("page", 0)
        .queryParam("size", pageSize)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    Assert.assertEquals(response.getTotalElements(), numberOfRequisitions);
    Assert.assertEquals(response.getNumberOfElements(), pageSize);
    Assert.assertTrue(response.isFirst());
    Assert.assertFalse(response.isLast());

    RequisitionDto previousRequisition = null;
    Set<UUID> userFacilities = fulfillmentFacilitiesReferenceDataService
        .getFulfillmentFacilities(user.getId(), ID)
        .stream().map(FacilityDto::getId).collect(Collectors.toSet());

    //Extract typed content from the PageImpl response
    ObjectMapper mapper = new ObjectMapper();
    mapper.findAndRegisterModules();
    List<RequisitionWithSupplyingDepotsDto> content = mapper.convertValue(response.getContent(),
        new TypeReference<List<RequisitionWithSupplyingDepotsDto>>() {
        });

    for (RequisitionWithSupplyingDepotsDto dto : content) {
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
    int numberOfRequisitions = 30;
    generateRequisitions(numberOfRequisitions);

    PageImplRepresentation<RequisitionWithSupplyingDepotsDto> response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam("filterValue", FACILITY)
        .queryParam("filterBy", FACILITY_CODE)
        .queryParam("sortBy", "programName")
        .queryParam("descending", Boolean.TRUE.toString())
        .queryParam("page", 0)
        .queryParam("size", numberOfRequisitions)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    Assert.assertEquals(response.getTotalElements(), numberOfRequisitions);
    Assert.assertEquals(response.getNumberOfElements(), numberOfRequisitions);
    Assert.assertTrue(response.isFirst());
    Assert.assertTrue(response.isLast());

    RequisitionDto previousRequisition = null;
    Set<UUID> userFacilities = fulfillmentFacilitiesReferenceDataService
        .getFulfillmentFacilities(user.getId(), ID)
        .stream().map(FacilityDto::getId).collect(Collectors.toSet());

    //Extract typed content from the PageImpl response
    ObjectMapper mapper = new ObjectMapper();
    mapper.findAndRegisterModules();
    List<RequisitionWithSupplyingDepotsDto> content = mapper.convertValue(response.getContent(),
        new TypeReference<List<RequisitionWithSupplyingDepotsDto>>() {
        });

    for (RequisitionWithSupplyingDepotsDto dto : content) {
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
        get(urlMatching("/api/users/" + UUID_REGEX + "/fulfillmentFacilities.*"))
            .willReturn(aResponse()
                .withHeader(CONTENT_TYPE, APPLICATION_JSON)
                .withBody(fulfillmentFacilitiesResult)));

    // when
    PageImplRepresentation<RequisitionWithSupplyingDepotsDto> response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    // then
    assertEquals(response.getContent().size(), 0);
  }

  @Test
  public void shouldGetApprovedRequisitionsWithUserFulfillmentRights() {
    // given
    int requisitionsAmount = 5;
    generateRequisitions(requisitionsAmount);

    wireMockRule.stubFor(get(
        urlMatching("/api/facilities/supplying.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody("[]")));

    for (Requisition requisition : requisitionRepository.findAll()) {
      if (requisition.getFacilityId().toString().equals(FACILITY_ID)) {
        ValueMatchingStrategy strategy = new ValueMatchingStrategy();
        strategy.setMatches(requisition.getProgramId().toString());

        // This mocks searching for supplying facilities
        wireMockRule.stubFor(get(
            urlMatching("/api/facilities/supplying.*"))
            .withQueryParam("programId", strategy)
            .willReturn(aResponse()
                .withHeader(CONTENT_TYPE, APPLICATION_JSON)
                .withBody(MOCK_SEARCH_SUPPLYING_FACILITY_RESULT)));
      }
    }

    // when
    PageImplRepresentation<RequisitionWithSupplyingDepotsDto> response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(APPROVED_REQUISITIONS_SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(PageImplRepresentation.class);

    // then
    assertEquals(requisitionsAmount, response.getNumberOfElements());

    //Extract typed content from the PageImpl response
    ObjectMapper mapper = new ObjectMapper();
    mapper.findAndRegisterModules();
    List<RequisitionWithSupplyingDepotsDto> content = mapper.convertValue(response.getContent(),
        new TypeReference<List<RequisitionWithSupplyingDepotsDto>>() {
        });

    for (RequisitionWithSupplyingDepotsDto dto : content) {
      Assert.assertTrue(dto.getRequisition().getStatus().equals(RequisitionStatus.APPROVED));
    }
  }

  @Test
  public void shouldNotInitiateIfFacilityDoesNotSupportProgram() throws Exception {
    UUID programId = UUID.randomUUID();
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam(PROGRAM, programId)
        .queryParam(FACILITY, facilityDto.getId())
        .queryParam(SUGGESTED_PERIOD, UUID.randomUUID())
        .queryParam(EMERGENCY, false)
        .when()
        .post(INITIATE_URL)
        .then()
        .statusCode(BAD_REQUEST.value())
        .body(MESSAGE, equalTo(getMessage(facilityDto.getId(), programId)));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotSubmitIfFacilityDoesNotSupportProgram() throws Exception {
    UUID programId = UUID.randomUUID();
    setProgramIdInRequisitionAndTemplate(programId);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .post(SUBMIT_URL)
        .then()
        .statusCode(BAD_REQUEST.value())
        .body(MESSAGE, equalTo(getMessage(getSharedFacilityId(), programId)));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotAuthorizeIfFacilityDoesNotSupportProgram() throws Exception {
    UUID programId = UUID.randomUUID();
    setProgramIdInRequisitionAndTemplate(programId);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", requisition.getId())
        .when()
        .post(AUTHORIZATION_URL)
        .then()
        .statusCode(BAD_REQUEST.value())
        .body(MESSAGE, equalTo(getMessage(getSharedFacilityId(), programId)));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotApproveIfFacilityDoesNotSupportProgram() throws Exception {
    UUID programId = UUID.randomUUID();
    setProgramIdInRequisitionAndTemplate(programId);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", requisition.getId())
        .when()
        .post(APPROVE_URL)
        .then()
        .statusCode(BAD_REQUEST.value())
        .body(MESSAGE, equalTo(getMessage(getSharedFacilityId(), programId)));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotInitiateIfUserHasNoRight() throws Exception {
    denyUserAllRights();

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .queryParam(PROGRAM, programDto.getId())
        .queryParam(FACILITY, facilityDto.getId())
        .queryParam(SUGGESTED_PERIOD, UUID.randomUUID())
        .queryParam(EMERGENCY, false)
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
    requisition.setProgramId(programDto.getId());
    requisition.setFacilityId(facilityDto.getId());
    requisition.setProcessingPeriodId(period.getId());
    requisition.setStatus(RequisitionStatus.APPROVED);
    requisition.setEmergency(false);
    requisition.setSupervisoryNodeId(supervisoryNode.getId());
    requisition.setTemplate(template);
    requisition.setNumberOfMonthsInPeriod(1);
    requisition.setCreatorId(user.getId());

    configurationSettingRepository.save(
        new ConfigurationSetting(REQUISITION_EMAIL_CONVERT_TO_ORDER_SUBJECT, "subject"));
    configurationSettingRepository.save(
        new ConfigurationSetting(REQUISITION_EMAIL_CONVERT_TO_ORDER_CONTENT, "content"));
    configurationSettingRepository.save(
        new ConfigurationSetting(REQUISITION_EMAIL_NOREPLY, "noreply@openlmis.org"));

    wireMockRule.stubFor(
        post(urlMatching("/api/orders.*"))
            .willReturn(aResponse().withStatus(200)));

    requisitionRepository.save(requisition);

    UUID supplyingFacility = FACILITY_UUID;
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
    UUID supplyingFacility = FACILITY_UUID;

    wireMockRule.stubFor(
        get(urlMatching("/api/users/" + UUID_REGEX + "/fulfillmentFacilities.*"))
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

  private Requisition configureRequisition(Requisition requisition) {
    requisition.setFacilityId(facilityDto.getId());
    requisition.setProcessingPeriodId(period.getId());
    requisition.setProgramId(programDto.getId());
    requisition.setCreatorId(user.getId());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setSupervisoryNodeId(supervisoryNode.getId());
    requisition.setCreatedDate(createdDate);
    requisition.setEmergency(false);
    requisition.setTemplate(template);
    requisition.setNumberOfMonthsInPeriod(1);

    return requisitionRepository.save(requisition);
  }

  private Requisition configureRequisitionForSearch(Requisition requisition) {
    requisition.setFacilityId(FACILITY_UUID);
    requisition.setProcessingPeriodId(PERIOD_UUID);
    requisition.setProgramId(PROGRAM_UUID);
    requisition.setCreatorId(user.getId());
    requisition.setSupervisoryNodeId(supervisoryNode.getId());
    requisition.setCreatedDate(createdDate);
    requisition.setEmergency(false);
    requisition.setTemplate(template);
    requisition.setNumberOfMonthsInPeriod(1);

    return requisitionRepository.save(requisition);
  }

  private Requisition generateRequisition(RequisitionStatus requisitionStatus, UUID facility) {
    Requisition requisition = new Requisition(facility, UUID.randomUUID(), UUID.randomUUID(),
        UUID.randomUUID(), requisitionStatus, true);
    requisition.setId(UUID.randomUUID());
    requisition.setCreatorId(user.getId());
    requisition.setCreatedDate(createdDate.now());
    requisition.setTemplate(template);
    requisition.setNumberOfMonthsInPeriod(1);
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
      column.setLabel(columnDefinition.getLabel());
      column.setIsDisplayed(true);

      if (!isEmpty(columnDefinition.getOptions())) {
        column.setOption(columnDefinition.getOptions().iterator().next());
      }
      if (!isEmpty(columnDefinition.getSources())) {
        column.setSource(columnDefinition.getSources().iterator().next());
      }

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

  private void mockSupervisoryNodeSearch() {
    wireMockRule.stubFor(
        get(urlMatching(SUPERVISORY_SEARCH_URL + ".*"))
            .willReturn(aResponse()
                .withHeader(CONTENT_TYPE, APPLICATION_JSON)
                .withBody("[{ \"id\":\"" + ID + "\"}]"))
    );
  }

  private void mockSupervisoryNode() {
    wireMockRule.stubFor(
        get(urlMatching(SUPERVISORY_URL + UUID_REGEX + ".*"))
            .willReturn(aResponse()
                .withHeader(CONTENT_TYPE, APPLICATION_JSON)
                .withBody("{"
                    + "\"id\":\"" + supervisoryNode.getId() + "\",\n"
                    + "\"code\":\"C1234\",\n"
                    + "\"name\":\"N1234\",\n"
                    + "\"description\":\"D1234\"\n"
                    + "}"))
    );
  }

  private void mockDetailedRoleAssignmentDto() {
    wireMockRule.stubFor(
        get(urlMatching(REFERENCEDATA_API_USERS + UUID_REGEX + "/roleAssignments.*"))
            .willReturn(aResponse()
                .withHeader(CONTENT_TYPE, APPLICATION_JSON)
                .withBody("[{ "
                    + "\"role\":{ \"rights\":" + MOCK_RIGHT_SEARCH + "},"
                    + " \"programId\":\"86191d25-4846-4775-a968-12df732e6004\","
                    + " \"supervisoryNodeId\":\"bb0c6821-df46-44d2-ba3f-48f613abe4c4\" }]"))
    );
  }

  private void mockFacility() {
    wireMockRule.stubFor(get(urlMatching("/api/facilities/aaf12a5a-8b16-11e6-ae22-56b6b6499611.*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody("{"
                + " \"id\":\"aaf12a5a-8b16-11e6-ae22-56b6b6499611\",\n"
                + " \"name\":\"facilityName\",\n"
                + " \"code\":\"facilityCode\",\n"
                + " \"active\":true,\n"
                + " \"enabled\":true,\n"
                + " \"geographicZone\":\"bf2b810b-cdbf-48b2-b569-149b3cf42387\","
                + " \"operator\":\"9456c3e9-c4a6-4a28-9e08-47ceb16a4121\","
                + " \"type\":\"ac1d268b-ce10-455f-bf87-9c667da8f060\""
                + "}")));
  }

  private void setProgramIdInRequisitionAndTemplate(UUID programId) {
    requisition.setProgramId(programId);
    requisitionRepository.save(requisition);
    template.setProgramId(requisition.getProgramId());
    requisitionTemplateRepository.save(template);
  }

  private String getMessage(UUID facilityId, UUID programId) {
    return messageSource.getMessage("requisition.error.facility-does-not-support-program",
        new Object[]{facilityId, programId}, LocaleContextHolder.getLocale());
  }
}
