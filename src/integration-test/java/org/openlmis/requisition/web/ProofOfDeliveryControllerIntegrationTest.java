package org.openlmis.requisition.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.apache.commons.io.IOUtils;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.openlmis.fulfillment.domain.ProofOfDeliveryLine;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
import org.openlmis.reporting.exception.ReportingException;
import org.openlmis.reporting.model.Template;
import org.openlmis.reporting.service.TemplateService;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProductDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
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
  private ProofOfDeliveryRepository proofOfDeliveryRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  private ProofOfDelivery proofOfDelivery = new ProofOfDelivery();
  private ProofOfDeliveryLine proofOfDeliveryLine = new ProofOfDeliveryLine();

  /**
   * Prepare the test environment.
   */
  @Before
  public void setUp() {

    ProductDto product = new ProductDto();
    product.setId(UUID.randomUUID());
    product.setPrimaryName("productName");
    product.setCode("productCode");
    product.setDispensingUnit("dispensingUnit");
    product.setPackSize(10);
    product.setPackRoundingThreshold(10);
    product.setActive(true);
    product.setFullSupply(false);
    product.setTracer(false);

    FacilityDto facility = new FacilityDto();
    facility.setId(UUID.randomUUID());
    facility.setCode("facilityCode");
    facility.setName("facilityName");
    facility.setDescription("facilityDescription");
    facility.setActive(true);
    facility.setEnabled(true);

    SupervisoryNodeDto supervisoryNode = new SupervisoryNodeDto();
    supervisoryNode.setId(UUID.randomUUID());
    supervisoryNode.setCode("NodeCode");
    supervisoryNode.setName("NodeName");
    supervisoryNode.setFacility(facility.getId());

    ProgramDto program = new ProgramDto();
    program.setId(UUID.randomUUID());
    program.setCode("programCode");

    ProcessingPeriodDto period = new ProcessingPeriodDto();
    period.setId(UUID.randomUUID());
    period.setProcessingSchedule(UUID.randomUUID());
    period.setName("periodName");
    period.setStartDate(LocalDate.of(2015, Month.JANUARY, 1));
    period.setEndDate(LocalDate.of(2015, Month.DECEMBER, 31));

    Requisition requisition = new Requisition();
    requisition.setProgram(program.getId());
    requisition.setFacility(facility.getId());
    requisition.setProcessingPeriod(period.getId());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setEmergency(false);
    requisition.setSupervisoryNode(supervisoryNode.getId());
    requisitionRepository.save(requisition);

    Order order = new Order();
    order.setStatus(OrderStatus.SHIPPED);
    order.setCreatedDate(LocalDateTime.now());
    order.setCreatedById(UUID.randomUUID());
    order.setOrderCode("O1");
    order.setProgram(program.getId());
    order.setQuotedCost(new BigDecimal(100));
    order.setSupplyingFacility(facility.getId());
    order.setRequestingFacility(facility.getId());
    order.setReceivingFacility(facility.getId());
    orderRepository.save(order);

    OrderLine orderLine = new OrderLine();
    orderLine.setOrder(order);
    orderLine.setProduct(product.getId());
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

  @Ignore
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
