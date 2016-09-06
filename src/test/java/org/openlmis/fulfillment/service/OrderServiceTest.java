package org.openlmis.fulfillment.service;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderNumberConfiguration;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderNumberConfigurationRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.domain.SupplyLine;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.SupplyLineRepository;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.hierarchyandsupervision.service.SupplyLineService;
import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.RequisitionService;

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
  private RequisitionService requisitionService;

  @Mock
  private SupplyLineService supplyLineService;

  @Mock
  private OrderRepository orderRepository;

  @Mock
  private UserRepository userRepository;

  @Mock
  private RequisitionRepository requisitionRepository;

  @Mock
  private OrderLineRepository orderLineRepository;

  @Mock
  private OrderNumberConfigurationRepository orderNumberConfigurationRepository;

  @Mock
  private SupplyLineRepository supplyLineRepository;

  @Mock
  private Program program;

  @InjectMocks
  private OrderService orderService;

  private List<Order> orders;
  private List<Requisition> requisitions;
  private List<SupplyLine> supplyLines;

  @Before
  public void setUp() {
    orders = new ArrayList<>();
    requisitions = new ArrayList<>();
    supplyLines = new ArrayList<>();
    generateInstances();
  }

  @Test
  public void shouldConvertRequisitionsToOrders() {
    User user = mock(User.class);
    UUID userId = UUID.randomUUID();
    when(user.getId()).thenReturn(userId);
    when(userRepository.findOne(userId)).thenReturn(user);

    for (int i = 0; i < requisitions.size(); i++) {
      when(requisitionRepository
              .findOne(requisitions.get(i).getId()))
              .thenReturn(requisitions.get(i));
      when(supplyLineService.searchSupplyLines(
              requisitions.get(i).getProgram(),
              requisitions.get(i).getSupervisoryNode()))
              .thenReturn(Arrays.asList(supplyLines.get(i)));
    }
    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration("prefix", true, true, true);
    when(orderNumberConfigurationRepository.findAll())
        .thenReturn(Arrays.asList(orderNumberConfiguration));
    when(program.getCode()).thenReturn("code");

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
              order.getReceivingFacility(),
              requisition.getFacility());
      assertEquals(
              order.getRequestingFacility(),
              requisition.getFacility());
      assertEquals(
              order.getProgram(),
              requisition.getProgram());
      assertEquals(
              order.getSupplyingFacility(),
              supplyLines.get(orders.indexOf(order)).getSupplyingFacility());
      assertEquals(1, order.getOrderLines().size());
      assertEquals(1, requisition.getRequisitionLines().size());

      OrderLine orderLine = order.getOrderLines().iterator().next();
      RequisitionLine requisitionLine = requisition.getRequisitionLines().iterator().next();
      assertEquals(requisitionLine.getRequestedQuantity().longValue(),
              orderLine.getOrderedQuantity().longValue());
      assertEquals(requisitionLine.getProduct().getId(), orderLine.getProduct().getId());
    }

    verify(requisitionRepository, atLeastOnce()).findOne(anyObject());
    verify(supplyLineService, atLeastOnce()).searchSupplyLines(anyObject(), anyObject());
    verify(orderRepository, atLeastOnce()).save(any(Order.class));
  }

  @Test
  public void shouldFindOrderIfMatchedSupplyingAndRequestingFacilitiesAndProgram() {
    Order order = orders.get(0);
    when(orderRepository
            .searchOrders(
                    order.getSupplyingFacility(),
                    order.getRequestingFacility(),
                    order.getProgram()))
            .thenReturn(Arrays.asList(order));

    List<Order> receivedOrders = orderService.searchOrders(
            order.getSupplyingFacility(),
            order.getRequestingFacility(),
            order.getProgram());

    assertEquals(1, receivedOrders.size());
    assertEquals(
            receivedOrders.get(0).getSupplyingFacility(),
            order.getSupplyingFacility());
    assertEquals(
            receivedOrders.get(0).getRequestingFacility(),
            order.getRequestingFacility());
    assertEquals(
            receivedOrders.get(0).getProgram(),
            order.getProgram());
    verify(orderRepository, atLeastOnce()).searchOrders(anyObject(), anyObject(), anyObject());
  }

  @Test
  public void shouldConvertOrderToCsvIfItExists() throws IOException, URISyntaxException {
    Order order = orders.get(0);
    when(order.getRequestingFacility().getCode()).thenReturn("FacilityCode");
    when(order.getOrderLines().get(0).getProduct().getPrimaryName()).thenReturn("product");
    when(order.getOrderLines().get(0).getProduct().getCode()).thenReturn("productCode");

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
    orderService.orderToCsv(order, header.toArray(new String[0]), writer);

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
      SupplyLine supplyLine = generateSupplyLine(
              requisition.getProgram(),
              requisition.getSupervisoryNode(),
              requisition.getSupervisoryNode().getFacility());
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
    order.setProgram(program);
    order.setCreatedDate(LocalDateTime.now().plusDays(instanceNumber));
    order.setCreatedBy(mock(User.class));
    order.setReceivingFacility(mock(Facility.class));
    order.setSupplyingFacility(mock(Facility.class));
    order.setRequestingFacility(mock(Facility.class));
    order.setQuotedCost(BigDecimal.valueOf(1));
    order.setOrderCode("OrderCode" + instanceNumber);
    order.setStatus(OrderStatus.ORDERED);
    List<OrderLine> orderLines = new ArrayList<>();
    orderLines.add(generateOrderLine(order));
    order.setOrderLines(orderLines);
    return order;
  }

  private Requisition generateRequisition() {
    Requisition requisition = new Requisition();
    requisition.setId(UUID.randomUUID());
    requisition.setFacility(mock(Facility.class));
    requisition.setProcessingPeriod(mock(ProcessingPeriod.class));
    requisition.setProgram(program);
    requisition.setCreatedDate(LocalDateTime.now());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setSupervisoryNode(mock(SupervisoryNode.class));
    requisition.setEmergency(true);
    List<RequisitionLine> requisitionLines = new ArrayList<>();
    requisitionLines.add(generateRequisitionLine());
    requisition.setRequisitionLines(requisitionLines);
    return requisition;
  }

  private OrderLine generateOrderLine(Order order) {
    OrderLine orderLine = new OrderLine();
    orderLine.setId(UUID.randomUUID());
    orderLine.setFilledQuantity(1000L);
    orderLine.setOrder(order);
    orderLine.setOrderedQuantity(1000L);
    orderLine.setProduct(mock(Product.class));
    return orderLine;
  }

  private RequisitionLine generateRequisitionLine() {
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setProduct(mock(Product.class));
    requisitionLine.setRequestedQuantity(1000);
    return requisitionLine;
  }

  private SupplyLine generateSupplyLine(
          Program program, SupervisoryNode supervisoryNode, Facility facility) {
    SupplyLine supplyLine = new SupplyLine();
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
}
