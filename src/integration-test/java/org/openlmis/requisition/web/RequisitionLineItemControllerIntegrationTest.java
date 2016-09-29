package org.openlmis.requisition.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.repository.RequisitionLineItemRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import guru.nidi.ramltester.junit.RamlMatchers;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionLineItemControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/requisitionLineItems";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String ACCESS_TOKEN = "access_token";
  private static final String REQUISITION = "requisition";
  private static final String PRODUCT = "product";
  private static final String TEST_CODE = "123";
  private static final String TEST_NAME = "Name";
  private static final Integer BEGINNING_BALANCE = 100;
  private static final Integer TOTAL_RECEIVED_QUANTITY = 200;
  private static final Integer TOTAL_LOSSES_AND_ADJUSTMENTS = 300;
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");

  @Autowired
  private RequisitionLineItemRepository requisitionLineItemRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  private RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
  private Requisition requisition = new Requisition();
  private ProcessingPeriodDto period = new ProcessingPeriodDto();
  private OrderableProductDto product = new OrderableProductDto();
  private ProgramDto program = new ProgramDto();
  private FacilityDto facility = new FacilityDto();

  @Before
  public void setUp() {
    requisitionLineItem = generateRequisitionLineItem();
  }

  @Test
  public void shouldFindRequisitionLineItems() {
    RequisitionLineItem[] response = restAssured.given()
        .queryParam(REQUISITION, requisition.getId())
        .queryParam(PRODUCT, product.getId())
        .queryParam(ACCESS_TOKEN, getToken())
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionLineItem[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertEquals(1, response.length);
    for ( RequisitionLineItem responseRequisitionLineItem : response ) {
      assertEquals(
          requisition.getId(),
          responseRequisitionLineItem.getRequisition().getId());
      assertEquals(
          product.getId(),
          responseRequisitionLineItem.getOrderableProduct());
      assertEquals(
          BEGINNING_BALANCE,
          responseRequisitionLineItem.getBeginningBalance());
      assertEquals(
          TOTAL_RECEIVED_QUANTITY,
          responseRequisitionLineItem.getTotalReceivedQuantity());
      assertEquals(
          TOTAL_LOSSES_AND_ADJUSTMENTS,
          responseRequisitionLineItem.getTotalLossesAndAdjustments());
      assertEquals(
          requisitionLineItem.getId(),
          responseRequisitionLineItem.getId());
    }
  }

  @Test
  public void shouldCreateRequisitionLineItem() {

    requisitionLineItemRepository.delete(requisitionLineItem);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(requisitionLineItem)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateRequisitionLineItem() {

    requisitionLineItem.setBeginningBalance(1);

    RequisitionLineItem response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", requisitionLineItem.getId())
          .body(requisitionLineItem)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(RequisitionLineItem.class);

    assertTrue(response.getBeginningBalance().equals(1));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllRequisitionLineItems() {

    RequisitionLineItem[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(RequisitionLineItem[].class);

    Iterable<RequisitionLineItem> requisitionLineItems = Arrays.asList(response);
    assertTrue(requisitionLineItems.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenRequisitionLineItem() {

    RequisitionLineItem response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", requisitionLineItem.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(RequisitionLineItem.class);

    assertTrue(requisitionLineItemRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonexistentRequisitionLineItem() {

    requisitionLineItemRepository.delete(requisitionLineItem);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", requisitionLineItem.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldDeleteRequisitionLineItem() {

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", requisitionLineItem.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(204);

    assertFalse(requisitionLineItemRepository.exists(requisitionLineItem.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteNonexistentRequisitionLineItem() {

    requisitionLineItemRepository.delete(requisitionLineItem);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", requisitionLineItem.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewRequisitionLineItemIfDoesNotExist() {

    requisitionLineItemRepository.delete(requisitionLineItem);
    requisitionLineItem.setBeginningBalance(1);

    RequisitionLineItem response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", ID)
          .body(requisitionLineItem)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(RequisitionLineItem.class);

    assertTrue(response.getBeginningBalance().equals(1));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateRequisitionLineItemIfStatusIsInitiated() {

    requisitionLineItem.setBeginningBalance(1);

    RequisitionLineItem response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", requisitionLineItem.getId())
          .body(requisitionLineItem)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(RequisitionLineItem.class);

    assertTrue(response.getBeginningBalance().equals(1));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateRequisitionLineItemIfStatusIsSubmitted() {

    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);
    requisitionLineItemRepository.save(requisitionLineItem);

    requisitionLineItem.setBeginningBalance(1);

    RequisitionLineItem response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", requisitionLineItem.getId())
          .body(requisitionLineItem)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(RequisitionLineItem.class);

    assertTrue(response.getBeginningBalance().equals(1));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotUpdateRequisitionLineItemIfStatusIsAuthorized() {

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);
    requisitionLineItemRepository.save(requisitionLineItem);

    requisitionLineItem.setBeginningBalance(1);

    RequisitionLineItem response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", requisitionLineItem.getId())
          .body(requisitionLineItem)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(RequisitionLineItem.class);

    assertFalse(response.getBeginningBalance().equals(1));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateRequisitionApprovedQuantityAndRemarksIfStatusIsAuthorized() {

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);
    requisitionLineItemRepository.save(requisitionLineItem);

    requisitionLineItem.setApprovedQuantity(1);
    requisitionLineItem.setRemarks("test");

    RequisitionLineItem response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", requisitionLineItem.getId())
          .body(requisitionLineItem)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(RequisitionLineItem.class);

    assertTrue(response.getApprovedQuantity().equals(1));
    assertEquals(response.getRemarks(), "test");
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private RequisitionLineItem generateRequisitionLineItem() {

    product.setId(UUID.randomUUID());

    program.setId(UUID.randomUUID());
    program.setCode(TEST_CODE);
    program.setPeriodsSkippable(true);


    facility.setId(UUID.randomUUID());
    facility.setCode(TEST_CODE);
    facility.setActive(true);
    facility.setEnabled(true);

    period.setId(UUID.randomUUID());
    period.setName(TEST_NAME);
    period.setProcessingSchedule(new ProcessingScheduleDto());
    period.setDescription("Description");
    period.setStartDate(LocalDate.of(2016, 1, 1));
    period.setEndDate(LocalDate.of(2016, 2, 1));

    requisitionLineItem.setOrderableProduct(product.getId());
    requisitionLineItem.setRequisition(requisition);
    requisitionLineItem.setRequestedQuantity(1);
    requisitionLineItem.setStockOnHand(1);
    requisitionLineItem.setTotalConsumedQuantity(1);
    requisitionLineItem.setBeginningBalance(BEGINNING_BALANCE);
    requisitionLineItem.setTotalReceivedQuantity(TOTAL_RECEIVED_QUANTITY);
    requisitionLineItem.setTotalLossesAndAdjustments(TOTAL_LOSSES_AND_ADJUSTMENTS);

    requisition.setFacility(facility.getId());
    requisition.setProcessingPeriod(period.getId());
    requisition.setProgram(program.getId());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setRequisitionLineItems(new ArrayList<>());
    requisition.getRequisitionLineItems().add(requisitionLineItem);
    requisition = requisitionRepository.save(requisition);

    return requisitionLineItem;
  }
}
