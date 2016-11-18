package org.openlmis.fulfillment.service;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

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
import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.SupplyLineDto;
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
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.UUID;

@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnusedPrivateField"})
@RunWith(MockitoJUnitRunner.class)
public class InternalOrderServiceTest {

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
  private InternalOrderService internalOrderService;

  @Before
  public void setUp() {
    generateMocks();
  }

  @Test
  public void shouldSaveOrder() throws RequisitionException {
    // given
    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration("prefix", true, true, true);
    when(orderNumberConfigurationRepository.findAll())
        .thenReturn(Collections.singletonList(orderNumberConfiguration));

    Requisition requisition = generateRequisition();
    Order order = Order.newOrder(OrderDto.newOrder(requisition));
    order.setCreatedById(UUID.randomUUID());

    when(requisitionRepository.findOne(requisition.getId())).thenReturn(requisition);

    // when
    Order created = internalOrderService.save(order);

    // then
    validateOrderBasedOnRequisition(created, requisition);
    verify(orderRepository).save(any(Order.class));
  }

  @Test
  public void shouldFindOrderIfMatchedSupplyingAndRequestingFacilitiesAndProgram() {
    // given
    Order order = generateOrder();

    when(orderRepository.searchOrders(
        order.getSupplyingFacilityId(), order.getRequestingFacilityId(), order.getProgramId()))
        .thenReturn(Collections.singletonList(order));

    // when
    List<Order> receivedOrders = internalOrderService.searchOrders(
        order.getSupplyingFacilityId(), order.getRequestingFacilityId(), order.getProgramId());

    // then
    assertEquals(1, receivedOrders.size());
    assertEquals(receivedOrders.get(0).getSupplyingFacilityId(), order.getSupplyingFacilityId());
    assertEquals(receivedOrders.get(0).getRequestingFacilityId(), order.getRequestingFacilityId());
    assertEquals(receivedOrders.get(0).getProgramId(), order.getProgramId());

    verify(orderRepository, atLeastOnce()).searchOrders(anyObject(), anyObject(), anyObject());
  }

  @Test
  public void shouldConvertOrderToCsvIfItExists()
      throws IOException, URISyntaxException, OrderCsvWriteException {
    // given
    Order order = generateOrder();
    when(order.getRequestingFacilityId()).thenReturn(UUID.randomUUID());

    //Creation date has to be static because we need to read expected csv from file
    order.setCreatedDate(ZonedDateTime.parse("2016-08-27T11:30Z").toLocalDateTime());

    List<String> header = new ArrayList<>();
    header.add(InternalOrderService.DEFAULT_COLUMNS[0]);
    header.add(InternalOrderService.DEFAULT_COLUMNS[1]);
    header.add(InternalOrderService.DEFAULT_COLUMNS[3]);
    header.add(InternalOrderService.DEFAULT_COLUMNS[4]);
    header.add(InternalOrderService.DEFAULT_COLUMNS[5]);

    OrderableProductDto orderableProductDto = mock(OrderableProductDto.class);
    when(orderableProductReferenceDataService.findOne(any())).thenReturn(orderableProductDto);
    when(orderableProductDto.getProductCode()).thenReturn("productCode");
    when(orderableProductDto.getName()).thenReturn("product");

    String expectedOutput = prepareExpectedCsvOutput(order, header);

    // when
    String receivedOutput;
    try (StringWriter writer = new StringWriter()) {
      internalOrderService.orderToCsv(order, header.toArray(new String[header.size()]), writer);
      receivedOutput = writer.toString().replace("\r\n", "\n");
    }

    // then
    assertEquals(expectedOutput, receivedOutput);
  }

  private Order generateOrder() {
    int number = new Random().nextInt();
    Order order = new Order();
    order.setProgramId(program.getId());
    order.setCreatedDate(LocalDateTime.now().plusDays(number));
    order.setCreatedById(UUID.randomUUID());
    order.setQuotedCost(BigDecimal.valueOf(1));
    order.setOrderCode("OrderCode " + number);
    order.setStatus(OrderStatus.ORDERED);
    order.setOrderLineItems(new ArrayList<>());
    order.getOrderLineItems().add(generateOrderLineItem(order));
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
    requisitionLineItem.setApprovedQuantity(0);
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

  private List<Requisition> setUpReleaseRequisitionsAsOrder(int amount)
      throws RequisitionException {
    if (amount < 1) {
      throw new IllegalArgumentException("Amount must be a positive number");
    }

    List<Requisition> requisitions = new ArrayList<>();

    for (int i = 0; i < amount; i++) {
      Requisition requisition = generateRequisition();
      SupplyLineDto supplyLine = generateSupplyLine(requisition.getProgramId(),
          requisition.getSupervisoryNodeId(), requisition.getFacilityId());

      requisitions.add(requisition);

      when(requisitionRepository.findOne(requisition.getId())).thenReturn(requisition);
      when(supplyLineService.search(requisition.getProgramId(), requisition.getSupervisoryNodeId()))
          .thenReturn(Collections.singletonList(supplyLine));
    }

    when(program.getCode()).thenReturn("code");
    when(requisitionService.releaseRequisitionsAsOrder(anyObject(), anyObject()))
        .thenReturn(requisitions);

    return requisitions;
  }

  private void validateOrderBasedOnRequisition(Order order, Requisition requisition) {
    assertEquals(OrderStatus.ORDERED, order.getStatus());
    assertEquals(order.getRequisitionId(), requisition.getId());
    assertEquals(order.getReceivingFacilityId(), requisition.getFacilityId());
    assertEquals(order.getRequestingFacilityId(), requisition.getFacilityId());
    assertEquals(order.getProgramId(), requisition.getProgramId());
    assertEquals(order.getSupplyingFacilityId(), requisition.getSupplyingFacilityId());
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

  private void generateMocks() {
    ProgramDto programDto = new ProgramDto();
    programDto.setCode("programCode");
    when(programReferenceDataService.findOne(any())).thenReturn(programDto);

    FacilityDto facilityDto = new FacilityDto();
    facilityDto.setCode("FacilityCode");
    when(facilityReferenceDataService.findOne(any())).thenReturn(facilityDto);

  }
}
