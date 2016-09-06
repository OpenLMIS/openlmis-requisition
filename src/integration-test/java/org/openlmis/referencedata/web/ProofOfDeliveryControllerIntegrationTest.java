package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.apache.commons.io.IOUtils;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.openlmis.fulfillment.domain.ProofOfDeliveryLine;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
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
import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProcessingSchedule;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.openlmis.referencedata.repository.GeographicLevelRepository;
import org.openlmis.referencedata.repository.GeographicZoneRepository;
import org.openlmis.referencedata.repository.ProcessingPeriodRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.repository.ProcessingScheduleRepository;
import org.openlmis.reporting.exception.ReportingException;
import org.openlmis.reporting.model.Template;
import org.openlmis.reporting.service.TemplateService;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import java.io.FileInputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

@SuppressWarnings("PMD.TooManyMethods")
public class ProofOfDeliveryControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/proofOfDeliveries";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String PRINT_URL = RESOURCE_URL + "/{id}/print";
  private static final String PRINT_POD = "Print POD";
  private static final String CONSISTENCY_REPORT = "Consistency Report";
  private static final String ACCESS_TOKEN = "access_token";
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");

  @Autowired
  private TemplateService templateService;

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private OrderLineRepository orderLineRepository;

  @Autowired
  private UserRepository userRepository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private ProcessingPeriodRepository periodRepository;

  @Autowired
  private ProcessingScheduleRepository scheduleRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  @Autowired
  private ProofOfDeliveryRepository proofOfDeliveryRepository;

  @Autowired
  private SupervisoryNodeRepository supervisoryNodeRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  private User user;
  private ProofOfDelivery proofOfDelivery = new ProofOfDelivery();
  private ProofOfDeliveryLine proofOfDeliveryLine = new ProofOfDeliveryLine();

  /**
   * Prepare the test environment.
   */
  @Before
  public void setUp() {
    assertEquals(1, userRepository.count());
    user = userRepository.findOne(INITIAL_USER_ID);

    ProductCategory productCategory = new ProductCategory();
    productCategory.setCode("PC");
    productCategory.setName("name");
    productCategory.setDisplayOrder(1);
    productCategoryRepository.save(productCategory);

    Product product = new Product();
    product.setPrimaryName("productName");
    product.setCode("productCode");
    product.setDispensingUnit("dispensingUnit");
    product.setDosesPerDispensingUnit(1);
    product.setPackSize(10);
    product.setPackRoundingThreshold(10);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(false);
    product.setTracer(false);
    product.setProductCategory(productCategory);
    productRepository.save(product);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode("F");
    facilityTypeRepository.save(facilityType);

    GeographicLevel geographicLevel = new GeographicLevel();
    geographicLevel.setCode("GL");
    geographicLevel.setLevelNumber(1);
    geographicLevelRepository.save(geographicLevel);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("geographicZoneCode");
    geographicZone.setLevel(geographicLevel);
    geographicZoneRepository.save(geographicZone);

    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("facilityCode");
    facility.setName("facilityName");
    facility.setDescription("facilityDescription");
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    SupervisoryNode supervisoryNode = new SupervisoryNode();
    supervisoryNode.setCode("NodeCode");
    supervisoryNode.setName("NodeName");
    supervisoryNode.setFacility(facility);
    supervisoryNodeRepository.save(supervisoryNode);

    Program program = new Program();
    program.setCode("programCode");
    programRepository.save(program);

    ProcessingSchedule schedule = new ProcessingSchedule();
    schedule.setCode("scheduleCode");
    schedule.setName("scheduleName");
    scheduleRepository.save(schedule);

    ProcessingPeriod period = new ProcessingPeriod();
    period.setProcessingSchedule(schedule);
    period.setName("periodName");
    period.setStartDate(LocalDate.of(2015, Month.JANUARY, 1));
    period.setEndDate(LocalDate.of(2015, Month.DECEMBER, 31));
    periodRepository.save(period);

    Requisition requisition = new Requisition();
    requisition.setProgram(program);
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(period);
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setEmergency(false);
    requisition.setSupervisoryNode(supervisoryNode);
    requisitionRepository.save(requisition);

    Order order = new Order();
    order.setStatus(OrderStatus.SHIPPED);
    order.setCreatedDate(LocalDateTime.now());
    order.setCreatedBy(user);
    order.setOrderCode("O1");
    order.setProgram(program);
    order.setQuotedCost(new BigDecimal(100));
    order.setSupplyingFacility(facility);
    order.setRequestingFacility(facility);
    order.setReceivingFacility(facility);
    orderRepository.save(order);

    OrderLine orderLine = new OrderLine();
    orderLine.setOrder(order);
    orderLine.setProduct(product);
    orderLine.setOrderedQuantity(100L);
    orderLine.setFilledQuantity(100L);
    orderLineRepository.save(orderLine);

    proofOfDeliveryLine.setOrderLine(orderLine);
    proofOfDeliveryLine.setProofOfDelivery(proofOfDelivery);
    proofOfDeliveryLine.setQuantityShipped(100L);
    proofOfDeliveryLine.setQuantityReturned(100L);
    proofOfDeliveryLine.setQuantityReceived(100L);
    proofOfDeliveryLine.setPackToShip(100L);
    proofOfDeliveryLine.setReplacedProductCode("replaced product code");
    proofOfDeliveryLine.setNotes("Notes");

    proofOfDelivery.setOrder(order);
    proofOfDelivery.setTotalShippedPacks(100);
    proofOfDelivery.setTotalReceivedPacks(100);
    proofOfDelivery.setTotalReturnedPacks(10);
    proofOfDelivery.setDeliveredBy("delivered by");
    proofOfDelivery.setReceivedBy("received by");
    proofOfDelivery.setReceivedDate(LocalDate.now());
    proofOfDelivery.setProofOfDeliveryLineItems(new ArrayList<>());
    proofOfDelivery.getProofOfDeliveryLineItems().add(proofOfDeliveryLine);
    proofOfDeliveryRepository.save(proofOfDelivery);
  }

  @Test
  public void shouldPrintProofOfDeliveryToPdf() throws IOException, ReportingException {
    ClassPathResource podReport = new ClassPathResource("reports/podPrint.jrxml");
    FileInputStream fileInputStream = new FileInputStream(podReport.getFile());
    MultipartFile templateOfProofOfDelivery = new MockMultipartFile("file",
        podReport.getFilename(), "multipart/form-data", IOUtils.toByteArray(fileInputStream));

    Template template = new Template(PRINT_POD, null, null, CONSISTENCY_REPORT, "");
    templateService.validateFileAndInsertTemplate(template, templateOfProofOfDelivery);

    restAssured.given()
        .pathParam("id", proofOfDelivery.getId())
        .queryParam(ACCESS_TOKEN, getToken())
        .when()
        .get(PRINT_URL)
        .then()
        .statusCode(200);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldDeleteProofOfDelivery() {

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", proofOfDelivery.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(204);

    assertFalse(proofOfDeliveryRepository.exists(proofOfDelivery.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateProofOfDelivery() {

    proofOfDeliveryRepository.delete(proofOfDelivery);
    proofOfDeliveryRepository.save(proofOfDelivery);
    proofOfDelivery.setTotalReceivedPacks(2);

    ProofOfDelivery response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", proofOfDelivery.getId())
          .body(proofOfDelivery)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(ProofOfDelivery.class);

    assertTrue(response.getTotalReceivedPacks().equals(2));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewProofOfDeliveryIfDoesNotExist() {

    proofOfDelivery.setTotalReceivedPacks(2);

    ProofOfDelivery response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", ID)
          .body(proofOfDelivery)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(ProofOfDelivery.class);

    assertTrue(response.getTotalReceivedPacks().equals(2));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllProofOfDeliveries() {

    ProofOfDelivery[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(ProofOfDelivery[].class);

    Iterable<ProofOfDelivery> proofOfDeliveries = Arrays.asList(response);
    assertTrue(proofOfDeliveries.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenProofOfDelivery() {

    ProofOfDelivery response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", proofOfDelivery.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(ProofOfDelivery.class);

    assertTrue(proofOfDeliveryRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonexistentProofOfDelivery() {

    proofOfDeliveryRepository.delete(proofOfDelivery);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", proofOfDelivery.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateProofOfDelivery() {
    proofOfDelivery.getProofOfDeliveryLineItems().clear();
    proofOfDeliveryRepository.delete(proofOfDelivery);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(proofOfDelivery)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
