package org.openlmis.requisition.web;

import guru.nidi.ramltester.junit.RamlMatchers;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.openlmis.fulfillment.domain.ProofOfDeliveryLine;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryLineRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.ProductDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.Month;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

@SuppressWarnings("PMD.TooManyMethods")
public class ProofOfDeliveryLineControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/proofOfDeliveryLines";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String ACCESS_TOKEN = "access_token";
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");
  private static final String NOTES = "OpenLMIS";

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private OrderLineRepository orderLineRepository;

  @Autowired
  private ProofOfDeliveryRepository proofOfDeliveryRepository;

  @Autowired
  private ProofOfDeliveryLineRepository proofOfDeliveryLineRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  UserReferenceDataService userReferenceDataService;

  private UserDto user;
  private ProofOfDelivery proofOfDelivery = new ProofOfDelivery();
  private ProofOfDeliveryLine proofOfDeliveryLine = new ProofOfDeliveryLine();

  /**
   * Prepare the test environment.
   */
  @Before
  public void setUp() {
    List<UserDto> allUsers = userReferenceDataService.findAll();
    Assert.assertEquals(1, allUsers.size());
    user = userReferenceDataService.findOne(INITIAL_USER_ID);

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
    supervisoryNode.setCode("NodeCode");
    supervisoryNode.setName("NodeName");
    supervisoryNode.setFacility(facility.getId());

    ProgramDto program = new ProgramDto();
    program.setId(UUID.randomUUID());
    program.setCode("programCode");

    ProcessingScheduleDto schedule = new ProcessingScheduleDto();
    schedule.setId(UUID.randomUUID());
    schedule.setCode("scheduleCode");
    schedule.setName("scheduleName");

    ProcessingPeriodDto period = new ProcessingPeriodDto();
    period.setId(UUID.randomUUID());
    period.setProcessingSchedule(schedule.getId());
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
    order.setRequisition(requisition);
    order.setOrderCode("O");
    order.setQuotedCost(new BigDecimal("10.00"));
    order.setStatus(OrderStatus.ORDERED);
    order.setProgram(program.getId());
    order.setCreatedById(user.getId());
    order.setRequestingFacility(facility.getId());
    order.setReceivingFacility(facility.getId());
    order.setSupplyingFacility(facility.getId());
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

  @Test
  public void shouldDeleteProofOfDeliveryLine() {

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", proofOfDeliveryLine.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(204);

    assertFalse(proofOfDeliveryLineRepository.exists(proofOfDeliveryLine.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteNonexistentProofOfDeliveryLine() {

    proofOfDeliveryLineRepository.delete(proofOfDeliveryLine);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", proofOfDeliveryLine.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllProofOfDeliveryLines() {

    ProofOfDeliveryLine[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(ProofOfDeliveryLine[].class);

    Iterable<ProofOfDeliveryLine> proofOfDeliveryLines = Arrays.asList(response);
    assertTrue(proofOfDeliveryLines.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenProofOfDeliveryLine() {

    ProofOfDeliveryLine response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", proofOfDeliveryLine.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(ProofOfDeliveryLine.class);

    assertTrue(proofOfDeliveryLineRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonexistentProofOfDeliveryLine() {

    proofOfDeliveryLineRepository.delete(proofOfDeliveryLine);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", proofOfDeliveryLine.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateProofOfDeliveryLine() {

    proofOfDeliveryLineRepository.delete(proofOfDeliveryLine);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(proofOfDeliveryLine)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateProofOfDeliveryLine() {

    proofOfDeliveryLine.setNotes(NOTES);

    ProofOfDeliveryLine response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", proofOfDeliveryLine.getId())
          .body(proofOfDeliveryLine)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(ProofOfDeliveryLine.class);

    assertEquals(response.getNotes(), NOTES);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewProofOfDeliveryLineIfDoesNotExist() {

    proofOfDeliveryLineRepository.delete(proofOfDeliveryLine);
    proofOfDeliveryLine.setNotes(NOTES);

    ProofOfDeliveryLine response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", ID)
          .body(proofOfDeliveryLine)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(ProofOfDeliveryLine.class);

    assertEquals(response.getNotes(), NOTES);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
