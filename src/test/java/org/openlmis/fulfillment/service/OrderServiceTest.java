package org.openlmis.fulfillment.service;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.domain.SupplyLine;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.SupplyLineRepository;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.hierarchyandsupervision.service.SupplyLineService;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProcessingSchedule;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.RequisitionService;

import java.io.IOException;
import java.math.BigDecimal;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

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
  private SupplyLineRepository supplyLineRepository;

  @Mock
  private User user;

  @Mock
  private Program program;

  @Mock
  private GeographicZone geographicZone;

  @Mock
  private FacilityType facilityType;

  @Mock
  private ProductCategory productCategory;

  @InjectMocks
  private OrderService orderService;

  private int currentInstanceNumber;

  private List<Order> orders;
  private List<Requisition> requisitions;
  private List<SupplyLine> supplyLines;

  @Before
  public void setUp() {
    orders = new ArrayList<>();
    requisitions = new ArrayList<>();
    supplyLines = new ArrayList<>();
    currentInstanceNumber = 0;
    generateInstances();
  }

  @Test
  public void shouldConvertRequisitionsToOrders() {
    for (int i = 0; i < requisitions.size(); i++) {
      when(requisitionRepository
              .findOne(requisitions.get(i).getId()))
              .thenReturn(requisitions.get(i));
      when(supplyLineService.searchSupplyLines(
              requisitions.get(i).getProgram(),
              requisitions.get(i).getSupervisoryNode()))
              .thenReturn(Arrays.asList(supplyLines.get(i)));
    }
    orders = orderService.convertToOrder(requisitions, user.getId());
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
              order.getReceivingFacility().getId(),
              requisition.getFacility().getId());
      assertEquals(
              order.getRequestingFacility().getId(),
              requisition.getFacility().getId());
      assertEquals(
              order.getProgram().getId(),
              requisition.getProgram().getId());
      assertEquals(
              order.getSupplyingFacility().getId(),
              supplyLines.get(orders.indexOf(order)).getSupplyingFacility().getId());
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
  }

  @Test
  public void shouldFindOrderIfMatchedSupplyingAndRequestingFacilitiesAndProgram() {
    when(orderRepository
            .searchOrders(
                    orders.get(0).getSupplyingFacility(),
                    orders.get(0).getRequestingFacility(),
                    orders.get(0).getProgram()))
            .thenReturn(Arrays.asList(orders.get(0)));

    List<Order> receivedOrders = orderService.searchOrders(
            orders.get(0).getSupplyingFacility(),
            orders.get(0).getRequestingFacility(),
            orders.get(0).getProgram());

    assertEquals(1, receivedOrders.size());
    assertEquals(
            receivedOrders.get(0).getSupplyingFacility().getId(),
            orders.get(0).getSupplyingFacility().getId());
    assertEquals(
            receivedOrders.get(0).getRequestingFacility().getId(),
            orders.get(0).getRequestingFacility().getId());
    assertEquals(
            receivedOrders.get(0).getProgram().getId(),
            orders.get(0).getProgram().getId());
    verify(orderRepository, atLeastOnce()).searchOrders(anyObject(), anyObject(), anyObject());
  }

  @Test
  public void shouldConvertOrderToCsvIfItExists() throws IOException, URISyntaxException {
    List<String> header = new ArrayList<>();
    header.add(OrderService.DEFAULT_COLUMNS[0]);
    header.add(OrderService.DEFAULT_COLUMNS[1]);
    header.add(OrderService.DEFAULT_COLUMNS[3]);
    header.add(OrderService.DEFAULT_COLUMNS[4]);
    header.add(OrderService.DEFAULT_COLUMNS[5]);

    //Creation date has to be static cuz we read expected csv from file
    Order order = orders.get(0);
    String time = "2016-08-27T11:30Z";
    ZonedDateTime zdt = ZonedDateTime.parse(time);
    LocalDateTime ldt = zdt.toLocalDateTime();
    order.setCreatedDate(ldt);

    String received = orderService.orderToCsv(
        orders.get(0), header.toArray(new String[0])).replace("\r\n","\n");
    String expected = prepareExpectedCsvOutput(orders.get(0), header);
    assertEquals(expected, received);
  }

  private void generateInstances() {
    generateOrders();
    generateRequisitions();
    generateSupplyLines();
  }

  private void generateRequisitions() {
    for (int requisitionCount = 0; requisitionCount < 2; requisitionCount++) {
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
    for (int instancesCount = 0; instancesCount < 5; instancesCount++) {
      orders.add(generateOrder());
    }
  }

  private Order generateOrder() {
    Order order = new Order();
    Integer instanceNumber = + generateInstanceNumber();
    order.setProgram(program);//generateProgram());
    order.setCreatedDate(LocalDateTime.now().plusDays(instanceNumber));
    order.setCreatedBy(user);//generateUser());
    order.setReceivingFacility(generateFacility());
    order.setSupplyingFacility(generateFacility());
    order.setRequestingFacility(generateFacility());
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
    requisition.setFacility(generateFacility());
    requisition.setProcessingPeriod(generatePeriod());
    requisition.setProgram(program);//generateProgram());
    requisition.setCreatedDate(LocalDateTime.now().plusDays(generateInstanceNumber()));
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setSupervisoryNode(generateSupervisoryNode());
    requisition.setEmergency(true);
    List<RequisitionLine> requisitionLines = new ArrayList<>();
    requisitionLines.add(generateRequisitionLines());
    requisition.setRequisitionLines(requisitionLines);
    return requisition;
  }

  private OrderLine generateOrderLine(Order order) {
    OrderLine orderLine = new OrderLine();
    orderLine.setId(UUID.randomUUID());
    orderLine.setFilledQuantity(Long.valueOf(1000));
    orderLine.setOrder(order);
    orderLine.setOrderedQuantity(Long.valueOf(1000));
    orderLine.setProduct(generateProduct());
    return orderLine;
  }

  private Product generateProduct() {
    Integer instanceNumber = generateInstanceNumber();
    Product product = new Product();
    product.setId(UUID.randomUUID());
    product.setCode("productCode");
    product.setPrimaryName("product");
    product.setDispensingUnit("unit" + instanceNumber);
    product.setDosesPerDispensingUnit(10);
    product.setPackSize(1);
    product.setPackRoundingThreshold(0);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(true);
    product.setTracer(false);
    product.setProductCategory(productCategory);//generateProductCategory());
    return product;
  }

  private RequisitionLine generateRequisitionLines() {
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setProduct(generateProduct());
    requisitionLine.setRequestedQuantity(1000);
    return requisitionLine;
  }

  private SupervisoryNode generateSupervisoryNode() {
    SupervisoryNode supervisoryNode = new SupervisoryNode();
    supervisoryNode.setId(UUID.randomUUID());
    supervisoryNode.setCode("SupervisoryNodeCode" + this.generateInstanceNumber());
    supervisoryNode.setFacility(generateFacility());
    return supervisoryNode;
  }

  private ProcessingPeriod generatePeriod() {
    ProcessingSchedule schedule = new ProcessingSchedule();
    Integer instanceNumber = generateInstanceNumber();
    schedule.setName("scheduleName" + instanceNumber);
    schedule.setCode("scheduleName" + instanceNumber);

    ProcessingPeriod period = new ProcessingPeriod();
    period.setId(UUID.randomUUID());
    period.setProcessingSchedule(schedule);
    period.setStartDate(LocalDate.now());
    period.setEndDate(LocalDate.now());
    period.setName("periodName" + instanceNumber);
    period.setDescription("description" + instanceNumber);
    return period;
  }

  private Facility generateFacility() {
    Integer instanceNumber = + generateInstanceNumber();
    Facility facility = new Facility();
    facility.setId(UUID.randomUUID());
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("FacilityCode");
    facility.setName("FacilityName" + instanceNumber);
    facility.setDescription("FacilityDescription" + instanceNumber);
    facility.setActive(true);
    facility.setEnabled(true);
    return facility;
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

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }

}
