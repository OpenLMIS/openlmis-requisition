//package org.openlmis.requisition.web;
//
//import guru.nidi.ramltester.junit.RamlMatchers;
//import org.junit.Before;
//import org.junit.Test;
//import org.openlmis.product.domain.Product;
//import org.openlmis.product.domain.ProductCategory;
//import org.openlmis.product.repository.ProductCategoryRepository;
//import org.openlmis.product.repository.ProductRepository;
//import org.openlmis.referencedata.domain.Facility;
//import org.openlmis.referencedata.domain.FacilityType;
//import org.openlmis.referencedata.domain.GeographicLevel;
//import org.openlmis.referencedata.domain.GeographicZone;
//import org.openlmis.referencedata.domain.ProcessingPeriod;
//import org.openlmis.referencedata.domain.Program;
//import org.openlmis.referencedata.domain.ProcessingSchedule;
//import org.openlmis.requisition.repository.FacilityRepository;
//import org.openlmis.requisition.repository.FacilityTypeRepository;
//import org.openlmis.requisition.repository.GeographicLevelRepository;
//import org.openlmis.requisition.repository.GeographicZoneRepository;
//import org.openlmis.requisition.repository.ProcessingPeriodRepository;
//import org.openlmis.requisition.repository.ProgramRepository;
//import org.openlmis.requisition.repository.ProcessingScheduleRepository;
//import org.openlmis.requisition.domain.Requisition;
//import org.openlmis.requisition.domain.RequisitionLine;
//import org.openlmis.requisition.domain.RequisitionStatus;
//import org.openlmis.requisition.repository.RequisitionLineRepository;
//import org.openlmis.requisition.repository.RequisitionRepository;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.http.MediaType;
//
//import java.time.LocalDate;
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
//public class RequisitionLineControllerIntegrationTest extends BaseWebIntegrationTest {
//
//  private static final String RESOURCE_URL = "/api/requisitionLines";
//  private static final String ID_URL = RESOURCE_URL + "/{id}";
//  private static final String SEARCH_URL = RESOURCE_URL + "/search";
//  private static final String ACCESS_TOKEN = "access_token";
//  private static final String REQUISITION = "requisition";
//  private static final String PRODUCT = "product";
//  private static final String TEST_CODE = "123";
//  private static final String TEST_NAME = "Name";
//  private static final Integer BEGINNING_BALANCE = 100;
//  private static final Integer TOTAL_RECEIVED_QUANTITY = 200;
//  private static final Integer TOTAL_LOSSES_AND_ADJUSTMENTS = 300;
//  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");
//
//  @Autowired
//  private RequisitionLineRepository requisitionLineRepository;
//
//  @Autowired
//  private ProductRepository productRepository;
//
//  @Autowired
//  private ProductCategoryRepository productCategoryRepository;
//
//  @Autowired
//  private ProcessingPeriodRepository periodRepository;
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
//  private FacilityRepository facilityRepository;
//
//  @Autowired
//  private ProgramRepository programRepository;
//
//  @Autowired
//  private ProcessingScheduleRepository scheduleRepository;
//
//  @Autowired
//  private RequisitionRepository requisitionRepository;
//
//  private RequisitionLine requisitionLine = new RequisitionLine();
//  private Requisition requisition = new Requisition();
//  private ProcessingPeriod period = new ProcessingPeriod();
//  private Product product = new Product();
//  private Program program = new Program();
//  private Facility facility = new Facility();
//
//  @Before
//  public void setUp() {
//    requisitionLine = generateRequisitionLine();
//  }
//
//  @Test
//  public void shouldFindRequisitionLines() {
//    RequisitionLine[] response = restAssured.given()
//        .queryParam(REQUISITION, requisition.getId())
//        .queryParam(PRODUCT, product.getId())
//        .queryParam(ACCESS_TOKEN, getToken())
//        .when()
//        .get(SEARCH_URL)
//        .then()
//        .statusCode(200)
//        .extract().as(RequisitionLine[].class);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//    assertEquals(1, response.length);
//    for ( RequisitionLine responseRequisitionLine : response ) {
//      assertEquals(
//          requisition.getId(),
//          responseRequisitionLine.getRequisition().getId());
//      assertEquals(
//          product.getId(),
//          responseRequisitionLine.getProduct().getId());
//      assertEquals(
//          BEGINNING_BALANCE,
//          responseRequisitionLine.getBeginningBalance());
//      assertEquals(
//          TOTAL_RECEIVED_QUANTITY,
//          responseRequisitionLine.getTotalReceivedQuantity());
//      assertEquals(
//          TOTAL_LOSSES_AND_ADJUSTMENTS,
//          responseRequisitionLine.getTotalLossesAndAdjustments());
//      assertEquals(
//          requisitionLine.getId(),
//          responseRequisitionLine.getId());
//    }
//  }
//
//  @Test
//  public void shouldCreateRequisitionLine() {
//
//    requisitionLineRepository.delete(requisitionLine);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .body(requisitionLine)
//          .when()
//          .post(RESOURCE_URL)
//          .then()
//          .statusCode(201);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldUpdateRequisitionLine() {
//
//    requisitionLine.setBeginningBalance(1);
//
//    RequisitionLine response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisitionLine.getId())
//          .body(requisitionLine)
//          .when()
//          .put(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(RequisitionLine.class);
//
//    assertTrue(response.getBeginningBalance().equals(1));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldGetAllRequisitionLines() {
//
//    RequisitionLine[] response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .when()
//          .get(RESOURCE_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(RequisitionLine[].class);
//
//    Iterable<RequisitionLine> requisitionLines = Arrays.asList(response);
//    assertTrue(requisitionLines.iterator().hasNext());
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldGetChosenRequisitionLine() {
//
//    RequisitionLine response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisitionLine.getId())
//          .when()
//          .get(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(RequisitionLine.class);
//
//    assertTrue(requisitionLineRepository.exists(response.getId()));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotGetNonexistentRequisitionLine() {
//
//    requisitionLineRepository.delete(requisitionLine);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisitionLine.getId())
//          .when()
//          .get(ID_URL)
//          .then()
//          .statusCode(404);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldDeleteRequisitionLine() {
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisitionLine.getId())
//          .when()
//          .delete(ID_URL)
//          .then()
//          .statusCode(204);
//
//    assertFalse(requisitionLineRepository.exists(requisitionLine.getId()));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotDeleteNonexistentRequisitionLine() {
//
//    requisitionLineRepository.delete(requisitionLine);
//
//    restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisitionLine.getId())
//          .when()
//          .delete(ID_URL)
//          .then()
//          .statusCode(404);
//
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldCreateNewRequisitionLineIfDoesNotExist() {
//
//    requisitionLineRepository.delete(requisitionLine);
//    requisitionLine.setBeginningBalance(1);
//
//    RequisitionLine response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", ID)
//          .body(requisitionLine)
//          .when()
//          .put(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(RequisitionLine.class);
//
//    assertTrue(response.getBeginningBalance().equals(1));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldUpdateRequisitionLineIfStatusIsInitiated() {
//
//    requisitionLine.setBeginningBalance(1);
//
//    RequisitionLine response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisitionLine.getId())
//          .body(requisitionLine)
//          .when()
//          .put(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(RequisitionLine.class);
//
//    assertTrue(response.getBeginningBalance().equals(1));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldUpdateRequisitionLineIfStatusIsSubmitted() {
//
//    requisition.setStatus(RequisitionStatus.SUBMITTED);
//    requisitionRepository.save(requisition);
//    requisitionLineRepository.save(requisitionLine);
//
//    requisitionLine.setBeginningBalance(1);
//
//    RequisitionLine response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisitionLine.getId())
//          .body(requisitionLine)
//          .when()
//          .put(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(RequisitionLine.class);
//
//    assertTrue(response.getBeginningBalance().equals(1));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldNotUpdateRequisitionLineIfStatusIsAuthorized() {
//
//    requisition.setStatus(RequisitionStatus.AUTHORIZED);
//    requisitionRepository.save(requisition);
//    requisitionLineRepository.save(requisitionLine);
//
//    requisitionLine.setBeginningBalance(1);
//
//    RequisitionLine response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisitionLine.getId())
//          .body(requisitionLine)
//          .when()
//          .put(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(RequisitionLine.class);
//
//    assertFalse(response.getBeginningBalance().equals(1));
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  @Test
//  public void shouldUpdateRequisitionApprovedQuantityAndRemarksIfStatusIsAuthorized() {
//
//    requisition.setStatus(RequisitionStatus.AUTHORIZED);
//    requisitionRepository.save(requisition);
//    requisitionLineRepository.save(requisitionLine);
//
//    requisitionLine.setApprovedQuantity(1);
//    requisitionLine.setRemarks("test");
//
//    RequisitionLine response = restAssured.given()
//          .queryParam(ACCESS_TOKEN, getToken())
//          .contentType(MediaType.APPLICATION_JSON_VALUE)
//          .pathParam("id", requisitionLine.getId())
//          .body(requisitionLine)
//          .when()
//          .put(ID_URL)
//          .then()
//          .statusCode(200)
//          .extract().as(RequisitionLine.class);
//
//    assertTrue(response.getApprovedQuantity().equals(1));
//    assertEquals(response.getRemarks(), "test");
//    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
//  }
//
//  private RequisitionLine generateRequisitionLine() {
//    ProductCategory productCategory1 = new ProductCategory();
//    productCategory1.setCode(TEST_CODE);
//    productCategory1.setName(TEST_NAME);
//    productCategory1.setDisplayOrder(1);
//    productCategoryRepository.save(productCategory1);
//
//    product.setCode(TEST_CODE);
//    product.setPrimaryName(TEST_NAME);
//    product.setDispensingUnit("Unit");
//    product.setDosesPerDispensingUnit(10);
//    product.setPackSize(1);
//    product.setPackRoundingThreshold(0);
//    product.setRoundToZero(false);
//    product.setActive(true);
//    product.setFullSupply(true);
//    product.setTracer(false);
//    product.setProductCategory(productCategory1);
//    product = productRepository.save(product);
//
//    program.setCode(TEST_CODE);
//    program.setPeriodsSkippable(true);
//    programRepository.save(program);
//
//    FacilityType facilityType = new FacilityType();
//    facilityType.setCode(TEST_CODE);
//    facilityTypeRepository.save(facilityType);
//
//    GeographicLevel level = new GeographicLevel();
//    level.setCode(TEST_CODE);
//    level.setLevelNumber(1);
//    geographicLevelRepository.save(level);
//
//    GeographicZone geographicZone = new GeographicZone();
//    geographicZone.setCode(TEST_CODE);
//    geographicZone.setLevel(level);
//    geographicZoneRepository.save(geographicZone);
//
//    facility.setType(facilityType);
//    facility.setGeographicZone(geographicZone);
//    facility.setCode(TEST_CODE);
//    facility.setActive(true);
//    facility.setEnabled(true);
//    facilityRepository.save(facility);
//
//    ProcessingSchedule schedule = new ProcessingSchedule();
//    schedule.setCode(TEST_CODE);
//    schedule.setName(TEST_NAME);
//    scheduleRepository.save(schedule);
//
//    period.setName(TEST_NAME);
//    period.setProcessingSchedule(schedule);
//    period.setDescription("Description");
//    period.setStartDate(LocalDate.of(2016, 1, 1));
//    period.setEndDate(LocalDate.of(2016, 2, 1));
//    periodRepository.save(period);
//
//    requisitionLine.setProduct(product);
//    requisitionLine.setRequisition(requisition);
//    requisitionLine.setRequestedQuantity(1);
//    requisitionLine.setStockOnHand(1);
//    requisitionLine.setTotalConsumedQuantity(1);
//    requisitionLine.setBeginningBalance(BEGINNING_BALANCE);
//    requisitionLine.setTotalReceivedQuantity(TOTAL_RECEIVED_QUANTITY);
//    requisitionLine.setTotalLossesAndAdjustments(TOTAL_LOSSES_AND_ADJUSTMENTS);
//
//    requisition.setFacility(facility);
//    requisition.setProcessingPeriod(period);
//    requisition.setProgram(program);
//    requisition.setStatus(RequisitionStatus.INITIATED);
//    requisition.setRequisitionLines(new ArrayList<>());
//    requisition.getRequisitionLines().add(requisitionLine);
//    requisition = requisitionRepository.save(requisition);
//
//    return requisitionLine;
//  }
//}
