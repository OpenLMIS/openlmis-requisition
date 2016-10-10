package org.openlmis.requisition.web;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.apache.commons.io.IOUtils;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLineItem;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.openlmis.fulfillment.domain.ProofOfDeliveryLineItem;
import org.openlmis.fulfillment.repository.OrderLineItemRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
import org.openlmis.reporting.exception.ReportingException;
import org.openlmis.reporting.model.Template;
import org.openlmis.reporting.service.TemplateService;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import guru.nidi.ramltester.junit.RamlMatchers;

import java.io.FileInputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.UUID;

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
  private OrderLineItemRepository orderLineItemRepository;

  @Autowired
  private ProofOfDeliveryRepository proofOfDeliveryRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  private ProofOfDelivery proofOfDelivery = new ProofOfDelivery();
  private ProofOfDeliveryLineItem proofOfDeliveryLineItem = new ProofOfDeliveryLineItem();

  /**
   * Prepare the test environment.
   */
  @Before
  public void setUp() {

    OrderableProductDto product = new OrderableProductDto();
    product.setId(UUID.randomUUID());

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
    supervisoryNode.setFacility(facility);

    ProgramDto program = new ProgramDto();
    program.setId(UUID.randomUUID());
    program.setCode("programCode");

    ProcessingPeriodDto period = new ProcessingPeriodDto();
    period.setId(UUID.randomUUID());
    period.setProcessingSchedule(new ProcessingScheduleDto());
    period.setName("periodName");
    period.setStartDate(LocalDate.of(2015, Month.JANUARY, 1));
    period.setEndDate(LocalDate.of(2015, Month.DECEMBER, 31));

    Requisition requisition = new Requisition();
    requisition.setProgramId(program.getId());
    requisition.setFacilityId(facility.getId());
    requisition.setProcessingPeriodId(period.getId());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setEmergency(false);
    requisition.setSupervisoryNodeId(supervisoryNode.getId());
    requisitionRepository.save(requisition);

    Order order = new Order();
    order.setStatus(OrderStatus.SHIPPED);
    order.setCreatedDate(LocalDateTime.now());
    order.setCreatedById(UUID.randomUUID());
    order.setOrderCode("O1");
    order.setProgramId(program.getId());
    order.setQuotedCost(new BigDecimal(100));
    order.setSupplyingFacilityId(facility.getId());
    order.setRequestingFacilityId(facility.getId());
    order.setReceivingFacilityId(facility.getId());
    orderRepository.save(order);

    OrderLineItem orderLineItem = new OrderLineItem();
    orderLineItem.setOrder(order);
    orderLineItem.setOrderableProductId(product.getId());
    orderLineItem.setOrderedQuantity(100L);
    orderLineItem.setFilledQuantity(100L);
    orderLineItemRepository.save(orderLineItem);

    proofOfDeliveryLineItem.setOrderLineItem(orderLineItem);
    proofOfDeliveryLineItem.setProofOfDelivery(proofOfDelivery);
    proofOfDeliveryLineItem.setQuantityShipped(100L);
    proofOfDeliveryLineItem.setQuantityReturned(100L);
    proofOfDeliveryLineItem.setQuantityReceived(100L);
    proofOfDeliveryLineItem.setPackToShip(100L);
    proofOfDeliveryLineItem.setReplacedProductCode("replaced product code");
    proofOfDeliveryLineItem.setNotes("Notes");

    proofOfDelivery.setOrder(order);
    proofOfDelivery.setTotalShippedPacks(100);
    proofOfDelivery.setTotalReceivedPacks(100);
    proofOfDelivery.setTotalReturnedPacks(10);
    proofOfDelivery.setDeliveredBy("delivered by");
    proofOfDelivery.setReceivedBy("received by");
    proofOfDelivery.setReceivedDate(LocalDate.now());
    proofOfDelivery.setProofOfDeliveryLineItems(new ArrayList<>());
    proofOfDelivery.getProofOfDeliveryLineItems().add(proofOfDeliveryLineItem);
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
