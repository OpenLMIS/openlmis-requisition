package org.openlmis.requisition.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import guru.nidi.ramltester.junit.RamlMatchers;
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
import org.openlmis.fulfillment.repository.ProofOfDeliveryLineItemRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
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
import org.springframework.http.MediaType;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.Month;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.UUID;

@Ignore
@SuppressWarnings("PMD.TooManyMethods")
public class ProofOfDeliveryLineItemControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/proofOfDeliveryLineItems";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String ACCESS_TOKEN = "access_token";
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");
  private static final String NOTES = "OpenLMIS";

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private OrderLineItemRepository orderLineItemRepository;

  @Autowired
  private ProofOfDeliveryRepository proofOfDeliveryRepository;

  @Autowired
  private ProofOfDeliveryLineItemRepository proofOfDeliveryLineItemRepository;

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
    supervisoryNode.setCode("NodeCode");
    supervisoryNode.setName("NodeName");
    supervisoryNode.setFacility(facility);

    ProgramDto program = new ProgramDto();
    program.setId(UUID.randomUUID());
    program.setCode("programCode");

    ProcessingScheduleDto schedule = new ProcessingScheduleDto();
    schedule.setId(UUID.randomUUID());
    schedule.setCode("scheduleCode");
    schedule.setName("scheduleName");

    ProcessingPeriodDto period = new ProcessingPeriodDto();
    period.setId(UUID.randomUUID());
    period.setProcessingSchedule(new ProcessingScheduleDto());
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
    order.setCreatedById(UUID.randomUUID());
    order.setRequestingFacility(facility.getId());
    order.setReceivingFacility(facility.getId());
    order.setSupplyingFacility(facility.getId());
    orderRepository.save(order);

    OrderLineItem orderLineItem = new OrderLineItem();
    orderLineItem.setOrder(order);
    orderLineItem.setOrderableProduct(product.getId());
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

  @Test
  public void shouldDeleteProofOfDeliveryLineItem() {

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", proofOfDeliveryLineItem.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(204);

    assertFalse(proofOfDeliveryLineItemRepository.exists(proofOfDeliveryLineItem.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteNonexistentProofOfDeliveryLineItem() {

    proofOfDeliveryLineItemRepository.delete(proofOfDeliveryLineItem);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", proofOfDeliveryLineItem.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllProofOfDeliveryLineItems() {

    ProofOfDeliveryLineItem[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(ProofOfDeliveryLineItem[].class);

    Iterable<ProofOfDeliveryLineItem> proofOfDeliveryLineItems = Arrays.asList(response);
    assertTrue(proofOfDeliveryLineItems.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenProofOfDeliveryLineItem() {

    ProofOfDeliveryLineItem response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", proofOfDeliveryLineItem.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(ProofOfDeliveryLineItem.class);

    assertTrue(proofOfDeliveryLineItemRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonexistentProofOfDeliveryLineItem() {

    proofOfDeliveryLineItemRepository.delete(proofOfDeliveryLineItem);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", proofOfDeliveryLineItem.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateProofOfDeliveryLineItem() {

    proofOfDeliveryLineItemRepository.delete(proofOfDeliveryLineItem);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(proofOfDeliveryLineItem)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateProofOfDeliveryLineItem() {

    proofOfDeliveryLineItem.setNotes(NOTES);

    ProofOfDeliveryLineItem response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", proofOfDeliveryLineItem.getId())
          .body(proofOfDeliveryLineItem)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(ProofOfDeliveryLineItem.class);

    assertEquals(response.getNotes(), NOTES);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewProofOfDeliveryLineItemIfDoesNotExist() {

    proofOfDeliveryLineItemRepository.delete(proofOfDeliveryLineItem);
    proofOfDeliveryLineItem.setNotes(NOTES);

    ProofOfDeliveryLineItem response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", ID)
          .body(proofOfDeliveryLineItem)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(ProofOfDeliveryLineItem.class);

    assertEquals(response.getNotes(), NOTES);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
