package org.openlmis.requisition.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLineItem;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.dto.ConvertToOrderDto;
import org.openlmis.fulfillment.repository.OrderLineItemRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import guru.nidi.ramltester.junit.RamlMatchers;

import java.math.BigDecimal;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
public class OrderControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/orders";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String ACCESS_TOKEN = "access_token";
  private static final String REQUESTING_FACILITY = "requestingFacility";
  private static final String SUPPLYING_FACILITY = "supplyingFacility";
  private static final String PROGRAM = "program";
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");
  private static final String NUMBER = "10.90";

  private UUID facility = UUID.randomUUID();
  private UUID facility1 = UUID.randomUUID();
  private UUID facility2 = UUID.randomUUID();
  private UUID program = UUID.fromString("aa66b58c-871a-11e6-ae22-56b6b6499611");
  private UUID program1 = UUID.randomUUID();
  private UUID program2 = UUID.randomUUID();
  private UUID period = UUID.randomUUID();
  private UUID period1 = UUID.randomUUID();
  private UUID period2 = UUID.randomUUID();
  private UUID product1 = UUID.randomUUID();
  private UUID product2 = UUID.randomUUID();
  private UUID supplyingFacility = UUID.fromString("1d5bdd9c-8702-11e6-ae22-56b6b6499611");
  private UUID supervisoryNode = UUID.randomUUID();
  private UUID user = UUID.fromString("35316636-6264-6331-2d34-3933322d3462");


  @Autowired
  private OrderLineItemRepository orderLineItemRepository;

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  private Order firstOrder = new Order();
  private Order secondOrder = new Order();
  private Order thirdOrder = new Order();
  private Requisition requisition;

  @Before
  public void setUp() {

    firstOrder = addOrder(null, "orderCode", UUID.randomUUID(), user, facility, facility, facility,
            OrderStatus.ORDERED, new BigDecimal("1.29"));

    Requisition requisition1 = addRequisition(program1, facility2, period1,
            RequisitionStatus.RELEASED, null);

    addRequisitionLineItem(requisition1, product1);
    requisition1 = requisitionRepository.findOne(requisition1.getId());

    Requisition requisition2 = addRequisition(program2, facility2, period2,
            RequisitionStatus.RELEASED, null);

    secondOrder = addOrder(requisition1, "O2", program1, user, facility2, facility2,
            facility1, OrderStatus.RECEIVED, new BigDecimal(100));

    thirdOrder = addOrder(requisition2, "O3", program2, user, facility2, facility2,
            facility1, OrderStatus.RECEIVED, new BigDecimal(200));

    addOrderLineItem(secondOrder, product1, 35L, 50L);

    addOrderLineItem(secondOrder, product2, 10L, 15L);

    addOrderLineItem(thirdOrder, product1, 50L, 50L);

    addOrderLineItem(thirdOrder, product2, 5L, 10L);

    OrderLineItem orderLineItem = addOrderLineItem(firstOrder, product1, 35L, 50L);

    List<OrderLineItem> orderLineItems = new ArrayList<>();
    orderLineItems.add(orderLineItem);
    firstOrder.setOrderLineItems(orderLineItems);

    firstOrder = orderRepository.save(firstOrder);

    requisition = addRequisition(program, facility, period,
            RequisitionStatus.APPROVED, supervisoryNode);
  }

  private Order addOrder(Requisition requisition, String orderCode, UUID program, UUID user,
                         UUID requestingFacility, UUID receivingFacility,
                         UUID supplyingFacility, OrderStatus orderStatus, BigDecimal cost) {
    Order order = new Order();
    order.setRequisition(requisition);
    order.setOrderCode(orderCode);
    order.setQuotedCost(cost);
    order.setStatus(orderStatus);
    order.setProgramId(program);
    order.setCreatedById(user);
    order.setRequestingFacilityId(requestingFacility);
    order.setReceivingFacilityId(receivingFacility);
    order.setSupplyingFacilityId(supplyingFacility);
    return orderRepository.save(order);
  }

  private Requisition addRequisition(UUID program, UUID facility, UUID processingPeriod,
                                     RequisitionStatus requisitionStatus, UUID supervisoryNode) {
    Requisition requisition = new Requisition();
    requisition.setProgramId(program);
    requisition.setFacilityId(facility);
    requisition.setProcessingPeriodId(processingPeriod);
    requisition.setStatus(requisitionStatus);
    requisition.setEmergency(false);
    requisition.setSupervisoryNodeId(supervisoryNode);

    return requisitionRepository.save(requisition);
  }

  private void addRequisitionLineItem(Requisition requisition, UUID product) {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequisition(requisition);
    requisitionLineItem.setOrderableProductId(product);
    requisitionLineItem.setRequestedQuantity(3);
    requisitionLineItem.setApprovedQuantity(3);
    requisition.setRequisitionLineItems(new ArrayList<>(
            Collections.singletonList(requisitionLineItem)));
  }

  private OrderLineItem addOrderLineItem(Order order, UUID product, Long filledQuantity,
                                 Long orderedQuantity) {
    OrderLineItem orderLineItem = new OrderLineItem();
    orderLineItem.setOrder(order);
    orderLineItem.setOrderableProductId(product);
    orderLineItem.setOrderedQuantity(orderedQuantity);
    orderLineItem.setFilledQuantity(filledQuantity);
    return orderLineItemRepository.save(orderLineItem);
  }

  @Test
  public void shouldNotFinalizeIfWrongOrderStatus() {
    firstOrder.setStatus(OrderStatus.SHIPPED);
    orderRepository.save(firstOrder);

    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .pathParam("id", firstOrder.getId().toString())
            .contentType("application/json")
            .when()
            .put("/api/orders/{id}/finalize")
            .then()
            .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.responseChecks());
  }

  @Test
  public void shouldPrintOrderAsCsv() {
    String csvContent = restAssured.given()
            .queryParam("format", "csv")
            .queryParam(ACCESS_TOKEN, getToken())
            .pathParam("id", secondOrder.getId())
            .when()
            .get("/api/orders/{id}/print")
            .then()
            .statusCode(200)
            .extract().body().asString();

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertTrue(csvContent.startsWith("productName,filledQuantity,orderedQuantity"));
  }

  @Test
  public void shouldPrintOrderAsPdf() {
    restAssured.given()
            .queryParam("format", "pdf")
            .queryParam(ACCESS_TOKEN, getToken())
            .pathParam("id", thirdOrder.getId().toString())
            .when()
            .get("/api/orders/{id}/print")
            .then()
            .statusCode(200);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldConvertRequisitionToOrder() {
    orderRepository.deleteAll();
    ConvertToOrderDto convertToOrderDto =
        new ConvertToOrderDto(requisition.getId(), supplyingFacility);

    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .body(Collections.singletonList(convertToOrderDto))
            .when()
            .post("/api/orders/requisitions")
            .then()
            .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertEquals(1, orderRepository.count());
    Order order = orderRepository.findAll().iterator().next();

    assertEquals(user, order.getCreatedById());
    assertEquals(OrderStatus.ORDERED, order.getStatus());
    assertEquals(order.getRequisition().getId(), requisition.getId());
    assertEquals(order.getReceivingFacilityId(), requisition.getFacilityId());
    assertEquals(order.getRequestingFacilityId(), requisition.getFacilityId());
    assertEquals(order.getProgramId(), requisition.getProgramId());
  }

  @Test
  public void shouldFindBySupplyingFacility() {
    Order[] response = restAssured.given()
            .queryParam(SUPPLYING_FACILITY, firstOrder.getSupplyingFacilityId())
            .queryParam(ACCESS_TOKEN, getToken())
            .when()
            .get(SEARCH_URL)
            .then()
            .statusCode(200)
            .extract().as(Order[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertEquals(1, response.length);
    for ( Order order : response ) {
      assertEquals(
              order.getSupplyingFacilityId(),
              firstOrder.getSupplyingFacilityId());
    }
  }

  @Test
  public void shouldFindBySupplyingFacilityAndRequestingFacility() {
    Order[] response = restAssured.given()
            .queryParam(SUPPLYING_FACILITY, firstOrder.getSupplyingFacilityId())
            .queryParam(REQUESTING_FACILITY, firstOrder.getRequestingFacilityId())
            .queryParam(ACCESS_TOKEN, getToken())
            .when()
            .get(SEARCH_URL)
            .then()
            .statusCode(200)
            .extract().as(Order[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertEquals(1, response.length);
    for ( Order order : response ) {
      assertEquals(
              order.getSupplyingFacilityId(),
              firstOrder.getSupplyingFacilityId());
      assertEquals(
              order.getRequestingFacilityId(),
              firstOrder.getRequestingFacilityId());
    }
  }

  @Test
  public void shouldFindBySupplyingFacilityAndRequestingFacilityAndProgram() {
    Order[] response = restAssured.given()
            .queryParam(SUPPLYING_FACILITY, firstOrder.getSupplyingFacilityId())
            .queryParam(REQUESTING_FACILITY, firstOrder.getRequestingFacilityId())
            .queryParam(PROGRAM, firstOrder.getProgramId())
            .queryParam(ACCESS_TOKEN, getToken())
            .when()
            .get(SEARCH_URL)
            .then()
            .statusCode(200)
            .extract().as(Order[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertEquals(1, response.length);
    for ( Order order : response ) {
      assertEquals(
              order.getSupplyingFacilityId(),
              firstOrder.getSupplyingFacilityId());
      assertEquals(
              order.getRequestingFacilityId(),
              firstOrder.getRequestingFacilityId());
      assertEquals(
              order.getProgramId(),
              firstOrder.getProgramId());
    }
  }

  @Test
  public void shouldDeleteOrder() {

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", firstOrder.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(204);

    assertFalse(orderRepository.exists(firstOrder.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteNonexistentOrder() {

    orderRepository.delete(firstOrder);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", firstOrder.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateOrder() {

    firstOrder.getOrderLineItems().clear();

    orderRepository.deleteAll();

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(firstOrder)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateOrder() {

    firstOrder.setQuotedCost(new BigDecimal(NUMBER));

    Order response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", firstOrder.getId())
          .body(firstOrder)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(Order.class);

    assertEquals(response.getQuotedCost(), new BigDecimal(NUMBER));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewOrderIfDoesNotExist() {

    orderRepository.delete(firstOrder);
    firstOrder.setQuotedCost(new BigDecimal(NUMBER));

    Order response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", ID)
          .body(firstOrder)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(Order.class);

    assertEquals(response.getQuotedCost(), new BigDecimal(NUMBER));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllOrders() {

    Order[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(Order[].class);

    Iterable<Order> orders = Arrays.asList(response);
    assertTrue(orders.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenOrder() {

    Order response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", firstOrder.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(Order.class);

    assertTrue(orderRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonexistentOrder() {

    orderRepository.delete(firstOrder);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", firstOrder.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturnConflictForExistingOrderCode() {
    firstOrder.getOrderLineItems().clear();

    orderRepository.save(firstOrder);
    firstOrder.setOrderLineItems(null);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(firstOrder)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(409);
  }

  @Test
  public void shouldExportOrderIfTypeIsNotSpecified() {
    String csvContent = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", secondOrder.getId())
        .when()
        .get("/api/orders/{id}/export")
        .then()
        .statusCode(200)
        .extract().body().asString();

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertTrue(csvContent.startsWith("Order number,Facility code,Product code,Product name,"
        + "Approved quantity,Period,Order date"));

    String orderDate = secondOrder.getCreatedDate().format(DateTimeFormatter.ofPattern("dd/MM/yy"));

    for (RequisitionLineItem lineItem : secondOrder.getRequisition().getRequisitionLineItems()) {
      assertTrue(csvContent.contains(secondOrder.getOrderCode()
          + ",facilityCode"
          + ",Product Code"
          + ",Product Name"
          + "," + lineItem.getApprovedQuantity()
          + ",03/16"
          + "," + orderDate));
    }
  }

  @Test
  public void shouldExportOrderIfTypeIsCsv() {
    String csvContent = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", secondOrder.getId())
        .queryParam("type", "csv")
        .when()
        .get("/api/orders/{id}/export")
        .then()
        .statusCode(200)
        .extract().body().asString();

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertTrue(csvContent.startsWith("Order number,Facility code,Product code,Product name,"
        + "Approved quantity,Period,Order date"));
  }

  @Test
  public void shouldNotExportOrderIfTypeIsDifferentThanCsv() {
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", secondOrder.getId())
        .queryParam("type", "pdf")
        .when()
        .get("/api/orders/{id}/export")
        .then()
        .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
