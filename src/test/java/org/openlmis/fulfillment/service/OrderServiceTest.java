package org.openlmis.fulfillment.service;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.hierarchyandsupervision.domain.SupplyLine;
import org.openlmis.hierarchyandsupervision.repository.SupplyLineRepository;
import org.openlmis.hierarchyandsupervision.service.SupplyLineService;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.RequisitionService;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnusedPrivateField"})
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

  @InjectMocks
  private OrderService orderService;

  private Integer currentInstanceNumber;

  private User user;
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
    initMocks(this);
  }

  @Test
  public void shouldConvertRequisitionsToOrders() {
    for (Order order : orders) {
      when(orderRepository
              .save(order))
              .thenReturn(order);
    }
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
  }

  @Test
  public void shouldConvertOrderToCsvIfItExists() {
    List<String> header = new ArrayList<>();
    header.add(OrderService.DEFAULT_COLUMNS[0]);
    header.add(OrderService.DEFAULT_COLUMNS[1]);
    header.add(OrderService.DEFAULT_COLUMNS[3]);
    header.add(OrderService.DEFAULT_COLUMNS[4]);
    header.add(OrderService.DEFAULT_COLUMNS[5]);

    String received = orderService.orderToCsv(orders.get(0), header.toArray(new String[0]));
    String expected = prepareExpectedCsvOutput(orders.get(0), header);
    assertEquals(expected,received);
  }

  private void generateInstances() {
    generateOrders();
    generateRequisitions();
    generateSupplyLines();
    user = generateUser();
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
    order.setProgram(generateProgram());
    order.setCreatedDate(LocalDateTime.now().plusDays(instanceNumber));
    order.setCreatedBy(generateUser());
    order.setReceivingFacility(generateFacility());
    order.setSupplyingFacility(generateFacility());
    order.setRequestingFacility(generateFacility());
    order.setQuotedCost(BigDecimal.valueOf(1));
    order.setOrderCode("OrderCode" + instanceNumber);
    order.setStatus(OrderStatus.ORDERED);
    Set<OrderLine> orderLines = new HashSet<>();
    orderLines.add(generateOrderLine(order));
    order.setOrderLines(orderLines);
    return order;
  }

  private Requisition generateRequisition() {
    Requisition requisition = new Requisition();
    requisition.setId(UUID.randomUUID());
    requisition.setFacility(generateFacility());
    requisition.setProcessingPeriod(generatePeriod());
    requisition.setProgram(generateProgram());
    requisition.setCreatedDate(LocalDateTime.now().plusDays(generateInstanceNumber()));
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setSupervisoryNode(generateSupervisoryNode());
    requisition.setEmergency(true);
    Set<RequisitionLine> requisitionLines = new HashSet<>();
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
    product.setCode("productCode" + instanceNumber);
    product.setPrimaryName("product" + instanceNumber);
    product.setDispensingUnit("unit" + instanceNumber);
    product.setDosesPerDispensingUnit(10);
    product.setPackSize(1);
    product.setPackRoundingThreshold(0);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(true);
    product.setTracer(false);
    product.setProductCategory(generateProductCategory());
    return product;
  }

  private ProductCategory generateProductCategory() {
    Integer instanceNumber = generateInstanceNumber();
    ProductCategory productCategory = new ProductCategory();
    productCategory.setId(UUID.randomUUID());
    productCategory.setCode("ProductCode" + instanceNumber);
    productCategory.setName("vaccine" + instanceNumber);
    productCategory.setDisplayOrder(1);
    return productCategory;
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

  private Period generatePeriod() {
    Schedule schedule = new Schedule();
    Integer instanceNumber = generateInstanceNumber();
    schedule.setName("scheduleName" + instanceNumber);
    schedule.setCode("scheduleName" + instanceNumber);

    Period period = new Period();
    period.setId(UUID.randomUUID());
    period.setProcessingSchedule(schedule);
    period.setStartDate(LocalDate.now());
    period.setEndDate(LocalDate.now());
    period.setName("periodName" + instanceNumber);
    period.setDescription("description" + instanceNumber);
    return period;
  }

  private Program generateProgram() {
    Program program = new Program();
    program.setId(UUID.randomUUID());
    program.setCode("ProgramCode" + generateInstanceNumber());
    program.setPeriodsSkippable(false);
    return program;
  }

  private User generateUser() {
    User user = new User();
    Integer instanceNumber = generateInstanceNumber();
    user.setFirstName("Ala" + instanceNumber);
    user.setLastName("ma" + instanceNumber);
    user.setUsername("kota" + instanceNumber);
    user.setPassword("iDobrze" + instanceNumber);
    user.setHomeFacility(generateFacility());
    user.setVerified(true);
    user.setActive(true);
    return user;
  }

  private Facility generateFacility() {
    Integer instanceNumber = + generateInstanceNumber();
    GeographicLevel geographicLevel = generateGeographicLevel();
    GeographicZone geographicZone = generateGeographicZone(geographicLevel);
    FacilityType facilityType = generateFacilityType();
    Facility facility = new Facility();
    facility.setId(UUID.randomUUID());
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("FacilityCode" + instanceNumber);
    facility.setName("FacilityName" + instanceNumber);
    facility.setDescription("FacilityDescription" + instanceNumber);
    facility.setActive(true);
    facility.setEnabled(true);
    return facility;
  }

  private GeographicZone generateGeographicZone(GeographicLevel geographicLevel) {
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("GeographicZone" + generateInstanceNumber());
    geographicZone.setLevel(geographicLevel);
    return geographicZone;
  }

  private GeographicLevel generateGeographicLevel() {
    GeographicLevel geographicLevel = new GeographicLevel();
    geographicLevel.setCode("GeographicLevel" + generateInstanceNumber());
    geographicLevel.setLevelNumber(1);
    return geographicLevel;
  }

  private FacilityType generateFacilityType() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode("FacilityType" + generateInstanceNumber());
    return facilityType;
  }

  private SupplyLine generateSupplyLine(
          Program program, SupervisoryNode supervisoryNode, Facility facility) {
    SupplyLine supplyLine = new SupplyLine();
    supplyLine.setProgram(program);
    supplyLine.setSupervisoryNode(supervisoryNode);
    supplyLine.setSupplyingFacility(facility);
    return supplyLine;
  }

  private String prepareExpectedCsvOutput(Order order, List<String> header) {
    String expected = "";
    for (int column = 0; column < header.size(); column++) {
      expected = expected + header.get(column) + ",";
    }
    expected = expected.substring(0, expected.length() - 1);
    expected = expected + "\r\n";
    for (OrderLine orderLine : order.getOrderLines()) {
      expected = expected
              + order.getRequestingFacility().getCode() + ","
              + order.getCreatedDate() + ","
              + orderLine.getProduct().getPrimaryName() + ","
              + orderLine.getProduct().getCode() + ","
              + orderLine.getOrderedQuantity() + ",";
      expected = expected.substring(0, expected.length() - 1);
      expected = expected + "\r\n";
    }
    return expected;
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }

}


