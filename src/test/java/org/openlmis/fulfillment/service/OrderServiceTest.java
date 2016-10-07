package org.openlmis.fulfillment.service;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLineItem;
import org.openlmis.fulfillment.domain.OrderNumberConfiguration;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.exception.OrderCsvWriteException;
import org.openlmis.fulfillment.repository.OrderLineItemRepository;
import org.openlmis.fulfillment.repository.OrderNumberConfigurationRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.SupplyLineDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupplyLineReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;

import java.io.IOException;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnusedPrivateField"})
@RunWith(MockitoJUnitRunner.class)
public class OrderServiceTest {

  @Mock
  private OrderableProductDto orderableProductDto;

  @Mock
  private RequisitionService requisitionService;

  @Mock
  private SupplyLineReferenceDataService supplyLineService;

  @Mock
  private UserReferenceDataService userService;

  @Mock
  private OrderRepository orderRepository;

  @Mock
  private RequisitionRepository requisitionRepository;

  @Mock
  private OrderLineItemRepository orderLineItemRepository;

  @Mock
  private OrderNumberConfigurationRepository orderNumberConfigurationRepository;

  @Mock
  private ProgramDto program;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  private OrderableProductReferenceDataService orderableProductReferenceDataService;

  @InjectMocks
  private OrderService orderService;

  private List<Order> orders;
  private List<Requisition> requisitions;
  private List<SupplyLineDto> supplyLines;

  @Before
  public void setUp() {
    orders = new ArrayList<>();
    requisitions = new ArrayList<>();
    supplyLines = new ArrayList<>();
    generateMocks();
    generateInstances();

  }

  @Test
  public void shouldConvertRequisitionsToOrders() throws RequisitionException {
    UserDto user = mock(UserDto.class);
    UUID userId = UUID.randomUUID();
    when(user.getId()).thenReturn(userId);
    when(userService.findOne(userId)).thenReturn(user);

    for (int i = 0; i < requisitions.size(); i++) {
      when(requisitionRepository
              .findOne(requisitions.get(i).getId()))
              .thenReturn(requisitions.get(i));
      when(supplyLineService.search(
              requisitions.get(i).getProgramId(),
              requisitions.get(i).getSupervisoryNodeId()))
              .thenReturn(Arrays.asList(supplyLines.get(i)));
    }
    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration("prefix", true, true, true);
    when(orderNumberConfigurationRepository.findAll())
        .thenReturn(Arrays.asList(orderNumberConfiguration));
    when(program.getCode()).thenReturn("code");

    when(requisitionService.releaseRequisitionsAsOrder(anyObject(), anyObject()))
        .thenReturn(requisitions);
    orders = orderService.convertToOrder(requisitions, userId);

    assertEquals(2, orders.size());
    for (Order order : orders) {
      Requisition requisition = requisitions.get(0);
      if (!requisition.getId().equals(order.getRequisition().getId())) {
        requisition = requisitions.get(1);
      }

      assertEquals(
              OrderStatus.ORDERED,
              order.getStatus());
      assertEquals(
              order.getRequisition().getId(),
              requisition.getId());
      assertEquals(
              order.getReceivingFacilityId(),
              requisition.getFacilityId());
      assertEquals(
              order.getRequestingFacilityId(),
              requisition.getFacilityId());
      assertEquals(
              order.getProgramId(),
              requisition.getProgramId());
      assertEquals(
              order.getSupplyingFacilityId(),
              requisition.getSupplyingFacilityId());
      assertEquals(1, order.getOrderLineItems().size());
      assertEquals(1, requisition.getRequisitionLineItems().size());

      OrderLineItem orderLineItem = order.getOrderLineItems().iterator().next();
      RequisitionLineItem requisitionLineItem =
          requisition.getRequisitionLineItems().iterator().next();
      assertEquals(requisitionLineItem.getRequestedQuantity().longValue(),
              orderLineItem.getOrderedQuantity().longValue());
      assertEquals(requisitionLineItem.getOrderableProductId(),
          orderLineItem.getOrderableProductId());
    }

    verify(orderRepository, atLeastOnce()).save(any(Order.class));
  }

  @Test
  public void shouldFindOrderIfMatchedSupplyingAndRequestingFacilitiesAndProgram() {
    Order order = orders.get(0);
    when(orderRepository
            .searchOrders(
                    order.getSupplyingFacilityId(),
                    order.getRequestingFacilityId(),
                    order.getProgramId()))
            .thenReturn(Arrays.asList(order));

    List<Order> receivedOrders = orderService.searchOrders(
            order.getSupplyingFacilityId(),
            order.getRequestingFacilityId(),
            order.getProgramId());

    assertEquals(1, receivedOrders.size());
    assertEquals(
            receivedOrders.get(0).getSupplyingFacilityId(),
            order.getSupplyingFacilityId());
    assertEquals(
            receivedOrders.get(0).getRequestingFacilityId(),
            order.getRequestingFacilityId());
    assertEquals(
            receivedOrders.get(0).getProgramId(),
            order.getProgramId());
    verify(orderRepository, atLeastOnce()).searchOrders(anyObject(), anyObject(), anyObject());
  }

  @Test
  public void shouldConvertOrderToCsvIfItExists()
          throws IOException, URISyntaxException, OrderCsvWriteException {
    Order order = orders.get(0);
    when(order.getRequestingFacilityId()).thenReturn(UUID.randomUUID());
    when(orderableProductReferenceDataService
        .findOne(any())).thenReturn(orderableProductDto);
    when(orderableProductDto.getProductCode()).thenReturn("productCode");
    when(orderableProductDto.getName()).thenReturn("product");

    //Creation date has to be static cuz we read expected csv from file
    ZonedDateTime zdt = ZonedDateTime.parse("2016-08-27T11:30Z");
    LocalDateTime ldt = zdt.toLocalDateTime();
    order.setCreatedDate(ldt);

    List<String> header = new ArrayList<>();
    header.add(OrderService.DEFAULT_COLUMNS[0]);
    header.add(OrderService.DEFAULT_COLUMNS[1]);
    header.add(OrderService.DEFAULT_COLUMNS[3]);
    header.add(OrderService.DEFAULT_COLUMNS[4]);
    header.add(OrderService.DEFAULT_COLUMNS[5]);

    StringWriter writer = new StringWriter();
    orderService.orderToCsv(order, header.toArray(new String[header.size()]), writer);

    String received = writer.toString().replace("\r\n","\n");
    String expected = prepareExpectedCsvOutput(order, header);
    assertEquals(expected, received);
  }

  private void generateInstances() {
    generateOrders();
    generateRequisitions();
    generateSupplyLines();
  }

  private void generateRequisitions() {
    final int requisitionCount = 2;
    for (int i = 0; i < requisitionCount; i++) {
      requisitions.add(generateRequisition());
    }
  }

  private void generateSupplyLines() {
    for (Requisition requisition : requisitions) {
      SupplyLineDto supplyLine = generateSupplyLine(
              requisition.getProgramId(),
              requisition.getSupervisoryNodeId(),
              requisition.getSupervisoryNodeId());
      supplyLines.add(supplyLine);
    }
  }

  private void generateOrders() {
    final int instancesCount = 5;
    for (int i = 0; i < instancesCount; i++) {
      orders.add(generateOrder(i));
    }
  }

  private Order generateOrder(int instanceNumber) {
    Order order = new Order();
    order.setProgramId(program.getId());
    order.setCreatedDate(LocalDateTime.now().plusDays(instanceNumber));
    order.setCreatedById(UUID.randomUUID());
    order.setQuotedCost(BigDecimal.valueOf(1));
    order.setOrderCode("OrderCode" + instanceNumber);
    order.setStatus(OrderStatus.ORDERED);
    List<OrderLineItem> orderLineItems = new ArrayList<>();
    orderLineItems.add(generateOrderLineItem(order));
    order.setOrderLineItems(orderLineItems);
    return order;
  }

  private Requisition generateRequisition() {
    Requisition requisition = new Requisition();
    requisition.setId(UUID.randomUUID());
    requisition.setProgramId(program.getId());
    requisition.setCreatedDate(LocalDateTime.now());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setEmergency(true);
    requisition.setSupplyingFacilityId(UUID.randomUUID());
    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(generateRequisitionLineItem());
    requisition.setRequisitionLineItems(requisitionLineItems);
    return requisition;
  }

  private OrderLineItem generateOrderLineItem(Order order) {
    OrderLineItem orderLineItem = new OrderLineItem();
    orderLineItem.setId(UUID.randomUUID());
    orderLineItem.setFilledQuantity(1000L);
    orderLineItem.setOrder(order);
    orderLineItem.setOrderedQuantity(1000L);
    return orderLineItem;
  }

  private RequisitionLineItem generateRequisitionLineItem() {
    RequisitionLineItem requisitionLineItem = new RequisitionLineItem();
    requisitionLineItem.setRequestedQuantity(1000);
    return requisitionLineItem;
  }

  private SupplyLineDto generateSupplyLine(
      UUID program, UUID supervisoryNode, UUID facility) {
    SupplyLineDto supplyLine = new SupplyLineDto();
    supplyLine.setProgram(program);
    supplyLine.setSupervisoryNode(supervisoryNode);
    supplyLine.setSupplyingFacility(facility);
    return supplyLine;
  }

  private String prepareExpectedCsvOutput(Order order, List<String> header)
      throws IOException, URISyntaxException {
    URL url =
        Thread.currentThread().getContextClassLoader().getResource("OrderServiceTest_expected.csv");
    byte[] encoded = Files.readAllBytes(Paths.get(url.getPath()));
    return new String(encoded, Charset.defaultCharset());
  }

  private void generateMocks() {
    ProgramDto programDto = new ProgramDto();
    programDto.setCode("programCode");
    when(programReferenceDataService.findOne(any())).thenReturn(programDto);

    FacilityDto facilityDto = new FacilityDto();
    facilityDto.setCode("FacilityCode");
    when(facilityReferenceDataService.findOne(any())).thenReturn(facilityDto);

  }
}
