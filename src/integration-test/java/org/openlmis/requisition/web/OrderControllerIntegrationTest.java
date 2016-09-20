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
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.math.BigDecimal;
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
  private UUID program = UUID.randomUUID();
  private UUID program1 = UUID.randomUUID();
  private UUID program2 = UUID.randomUUID();
  private UUID period = UUID.randomUUID();
  private UUID period1 = UUID.randomUUID();
  private UUID period2 = UUID.randomUUID();
  private UUID product1 = UUID.randomUUID();
  private UUID product2 = UUID.randomUUID();
  private UUID supplyingFacility = UUID.randomUUID();
  private UUID supervisoryNode = UUID.randomUUID();
  private UUID user = UUID.randomUUID();


  @Autowired
  private OrderLineRepository orderLineRepository;

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;
  @Autowired
  private RequisitionLineRepository requisitionLineRepository;

  private Order firstOrder = new Order();
  private Order secondOrder = new Order();
  private Order thirdOrder = new Order();
  private Requisition requisition;

  @Before
  public void setUp() {
    firstOrder = addOrder(null, "orderCode", UUID.randomUUID(), user, facility, facility, facility,
            OrderStatus.ORDERED, new BigDecimal("1.29"));

    Requisition requisition1 = addRequisition(program1, facility1, period1,
            RequisitionStatus.RELEASED, null);

    addRequisitionLine(requisition1, product1);
    requisition1 = requisitionRepository.findOne(requisition1.getId());

    Requisition requisition2 = addRequisition(program2, facility1, period2,
            RequisitionStatus.RELEASED, null);

    secondOrder = addOrder(requisition1, "O2", program1, user, facility2, facility2,
            facility1, OrderStatus.RECEIVED, new BigDecimal(100));

    thirdOrder = addOrder(requisition2, "O3", program2, user, facility2, facility2,
            facility1, OrderStatus.RECEIVED, new BigDecimal(200));

    addOrderLine(secondOrder, product1, 35L, 50L);

    addOrderLine(secondOrder, product2, 10L, 15L);

    addOrderLine(thirdOrder, product1, 50L, 50L);

    addOrderLine(thirdOrder, product2, 5L, 10L);

    OrderLine orderLine = addOrderLine(firstOrder, product1, 35L, 50L);

    List<OrderLine> orderLines = new ArrayList<>();
    orderLines.add(orderLine);
    firstOrder.setOrderLines(orderLines);

    firstOrder = orderRepository.save(firstOrder);

    requisition = addRequisition(program, supplyingFacility, period,
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
    order.setProgram(program);
    order.setCreatedById(user);
    order.setRequestingFacility(requestingFacility);
    order.setReceivingFacility(receivingFacility);
    order.setSupplyingFacility(supplyingFacility);
    return orderRepository.save(order);
  }

  private Requisition addRequisition(UUID program, UUID facility,
                                     UUID processingPeriod,
                                     RequisitionStatus requisitionStatus,
                                     UUID supervisoryNode) {
    Requisition requisition = new Requisition();
    requisition.setProgram(program);
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(processingPeriod);
    requisition.setStatus(requisitionStatus);
    requisition.setEmergency(false);
    requisition.setSupervisoryNode(supervisoryNode);

    return requisitionRepository.save(requisition);
  }

  private RequisitionLine addRequisitionLine(Requisition requisition, UUID product) {
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequisition(requisition);
    requisitionLine.setProduct(product);
    requisitionLine.setRequestedQuantity(3);
    requisitionLine.setApprovedQuantity(3);

    return requisitionLineRepository.save(requisitionLine);
  }

  private OrderLine addOrderLine(Order order, UUID product, Long filledQuantity,
                                 Long orderedQuantity) {
    OrderLine orderLine = new OrderLine();
    orderLine.setOrder(order);
    orderLine.setProduct(product);
    orderLine.setOrderedQuantity(orderedQuantity);
    orderLine.setFilledQuantity(filledQuantity);
    return orderLineRepository.save(orderLine);
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
  @Ignore
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
    //for (OrderLine o : orderRepository.findOne(secondOrder.getId()).getOrderLines()) {
    //      assertTrue(csvContent.contains(o.getProduct().getPrimaryName()
    //              + "," + o.getFilledQuantity()
    //              + "," + o.getOrderedQuantity()));
    //}
  }

  @Ignore
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

  @Ignore
  @Test
  public void shouldConvertRequisitionToOrder() {
    orderRepository.deleteAll();

    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .body(Collections.singletonList(requisition))
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
    assertEquals(order.getReceivingFacility(), requisition.getFacility());
    assertEquals(order.getRequestingFacility(), requisition.getFacility());
    assertEquals(order.getProgram(), requisition.getProgram());
  }

  @Test
  public void shouldFindBySupplyingFacility() {
    Order[] response = restAssured.given()
            .queryParam(SUPPLYING_FACILITY, firstOrder.getSupplyingFacility())
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
              order.getSupplyingFacility(),
              firstOrder.getSupplyingFacility());
    }
  }

  @Test
  public void shouldFindBySupplyingFacilityAndRequestingFacility() {
    Order[] response = restAssured.given()
            .queryParam(SUPPLYING_FACILITY, firstOrder.getSupplyingFacility())
            .queryParam(REQUESTING_FACILITY, firstOrder.getRequestingFacility())
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
              order.getSupplyingFacility(),
              firstOrder.getSupplyingFacility());
      assertEquals(
              order.getRequestingFacility(),
              firstOrder.getRequestingFacility());
    }
  }

  @Test
  public void shouldFindBySupplyingFacilityAndRequestingFacilityAndProgram() {
    Order[] response = restAssured.given()
            .queryParam(SUPPLYING_FACILITY, firstOrder.getSupplyingFacility())
            .queryParam(REQUESTING_FACILITY, firstOrder.getRequestingFacility())
            .queryParam(PROGRAM, firstOrder.getProgram())
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
              order.getSupplyingFacility(),
              firstOrder.getSupplyingFacility());
      assertEquals(
              order.getRequestingFacility(),
              firstOrder.getRequestingFacility());
      assertEquals(
              order.getProgram(),
              firstOrder.getProgram());
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

    firstOrder.getOrderLines().clear();
    orderRepository.delete(firstOrder);

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
/*
  @Test
  public void shouldExportOrderToCsv() {
    String csvContent = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", secondOrder.getId())
        .when()
        .get("/api/orders/{id}/csv")
        .then()
        .statusCode(200)
        .extract().body().asString();

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertTrue(csvContent.startsWith("Order number,Facility code,Product code,Product name,"
        + "Approved quantity,Period,Order date"));

    String period = secondOrder.getRequisition().getProcessingPeriod().getStartDate().format(
        DateTimeFormatter.ofPattern("MM/yy"));
    String orderDate = secondOrder.getCreatedDate().format(
        DateTimeFormatter.ofPattern("dd/MM/yy"));

    for (RequisitionLine line : secondOrder.getRequisition().getRequisitionLines()) {
      assertTrue(csvContent.contains(secondOrder.getOrderCode()
          + "," + secondOrder.getRequisition().getFacility().getCode()
          + "," + line.getProduct().getCode()
          + "," + line.getProduct().getPrimaryName()
          + "," + line.getApprovedQuantity()
          + "," + period
          + "," + orderDate));
    }
  }*/
}
