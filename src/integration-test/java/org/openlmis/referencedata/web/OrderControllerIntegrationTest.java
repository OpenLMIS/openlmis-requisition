package org.openlmis.referencedata.web;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.SupervisoryNodeRepository;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.domain.Stock;
import org.openlmis.referencedata.domain.StockInventory;
import org.openlmis.referencedata.domain.SupplyLine;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.openlmis.referencedata.repository.GeographicLevelRepository;
import org.openlmis.referencedata.repository.GeographicZoneRepository;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.openlmis.referencedata.repository.StockInventoryRepository;
import org.openlmis.referencedata.repository.StockRepository;
import org.openlmis.referencedata.repository.SupplyLineRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponents;
import org.springframework.web.util.UriComponentsBuilder;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.Month;
import java.util.Collections;

@SuppressWarnings("PMD.TooManyMethods")
public class OrderControllerIntegrationTest extends BaseWebIntegrationTest {

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private OrderLineRepository orderLineRepository;

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private UserRepository userRepository;

  @Autowired
  private StockRepository stockRepository;

  @Autowired
  private StockInventoryRepository stockInventoryRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  @Autowired
  private SupervisoryNodeRepository supervisoryNodeRepository;

  @Autowired
  private SupplyLineRepository supplyLineRepository;

  private static final String RESOURCE_FINALIZE_URL = BASE_URL + "/api/orders/{id}/finalize";

  private static final String RESOURCE_URL = BASE_URL + "/api/orders";

  private static final String USERNAME = "testUser";

  private Order firstOrder = new Order();
  private Order secondOrder = new Order();
  private Order thirdOrder = new Order();
  private Product firstProduct = new Product();
  private Product secondProduct = new Product();
  private StockInventory firstStockInventory = new StockInventory();
  private User firstUser = new User();
  private Requisition requisition;
  private SupplyLine supplyLine;
  private User user;

  /** Prepare the test environment. */
  @Before
  public void setUp() {
    cleanUp();

    firstStockInventory.setName("stockInventoryName");
    stockInventoryRepository.save(firstStockInventory);

    GeographicLevel geographicLevel = addGeographicLevel("geographicLevelCode", 1);

    FacilityType facilityType = addFacilityType("facilityTypeCode");

    GeographicZone geographicZone = addGeographicZone("geographicZoneCode", geographicLevel);

    Facility facility = addFacility("facilityName", "facilityCode", "facilityDescription",
                                    facilityType, geographicZone, firstStockInventory, true, true);

    Program program = addProgram("programCode");

    Assert.assertEquals(1, userRepository.count());
    user = userRepository.findAll().iterator().next();

    firstOrder = addOrder(null, "orderCode", program, user, facility, facility, facility,
                          OrderStatus.ORDERED, new BigDecimal("1.29"));

    ProductCategory productCategory1 = addProductCategory("PC1", "PC1 name", 1);

    ProductCategory productCategory2 = addProductCategory("PC2", "PC2 name", 2);

    firstProduct = addProduct("firstProductName", "firstProductCode", "unit",
                              10, 1, 0, false, true, true, false, productCategory1);

    secondProduct = addProduct("secondProductName", "secondProductCode", "unit",
                               10, 1, 0, false, true, true, false, productCategory2);

    Schedule schedule1 = addSchedule("Schedule1", "S1");

    Schedule schedule2 = addSchedule("Schedule2", "S2");

    Program program1 = addProgram("P1");

    Program program2 = addProgram("P2");

    GeographicLevel geographicLevel1 = addGeographicLevel("GL1", 1);

    GeographicLevel geographicLevel2 = addGeographicLevel("GL2", 1);

    GeographicZone geographicZone1 = addGeographicZone("GZ1", geographicLevel1);

    GeographicZone geographicZone2 = addGeographicZone("GZ2", geographicLevel2);

    FacilityType facilityType1 = addFacilityType("FT1");

    FacilityType facilityType2 = addFacilityType("FT2");

    Period period1 = addPeriod("P1", schedule1, LocalDate.of(2015, Month.JANUARY, 1),
            LocalDate.of(2015, Month.DECEMBER, 31));

    Period period2 = addPeriod("P2", schedule2, LocalDate.of(2016, Month.JANUARY, 1),
            LocalDate.of(2016, Month.DECEMBER, 31));

    Facility facility1 = addFacility("facility1", "F1", null, facilityType1,
                                     geographicZone1, null, true, false);

    Facility facility2 = addFacility("facility2", "F2", null, facilityType2,
                                     geographicZone2, null, true, false);

    Requisition requisition1 = addRequisition(program1, facility1, period1,
                                              RequisitionStatus.RELEASED, null);

    Requisition requisition2 = addRequisition(program2, facility1, period2,
                                              RequisitionStatus.RELEASED, null);

    firstUser = addUser(USERNAME, "pass", "Alice", "Cat", facility1);

    secondOrder = addOrder(requisition1, "O2", program1, firstUser, facility2, facility2,
                           facility1, OrderStatus.RECEIVED, new BigDecimal(100));

    thirdOrder = addOrder(requisition2, "O3", program2, firstUser, facility2, facility2,
                          facility1, OrderStatus.RECEIVED, new BigDecimal(200));

    ProductCategory productCategory3 = addProductCategory("PCCode1", "PCName1", 1);

    ProductCategory productCategory4 = addProductCategory("PCCode2", "PCName2", 2);

    Product product1 = addProduct("Product1", "P1", "pill", 1, 10, 10, false, true, false, false,
            productCategory3);

    Product product2 = addProduct("Product2", "P2", "pill", 2, 20, 20, true, true, false, false,
            productCategory4);

    addOrderLine(secondOrder, product1, 35L, 50L,
                                        null, null, null, null);

    addOrderLine(secondOrder, product2, 10L, 15L,
            null, null, null, null);

    addOrderLine(thirdOrder, product1, 50L, 50L,
            null, null, null, null);

    addOrderLine(thirdOrder, product2, 5L, 10L,
            null, null, null, null);

    geographicLevel = addGeographicLevel("levelCode", 1);

    facilityType = addFacilityType("typeCode");

    geographicZone = addGeographicZone("zoneCode", geographicLevel);

    Facility supplyingFacility = addFacility("supplyingFacilityName", "supplyingFacilityCode",
        "description", facilityType, geographicZone, null, true, true);

    SupervisoryNode supervisoryNode = addSupervisoryNode(supplyingFacility);

    program = addProgram("progCode");

    Schedule schedule = addSchedule("Schedule3", "S3");

    Period period = addPeriod("P3", schedule, LocalDate.of(2015, Month.JANUARY, 1),
        LocalDate.of(2015, Month.DECEMBER, 31));

    requisition = addRequisition(program, supplyingFacility, period,
        RequisitionStatus.APPROVED, supervisoryNode);

    supplyLine = addSupplyLine(supervisoryNode, program, supplyingFacility);
  }

  /**
   * Cleanup the test environment.
   */
  @After
  public void cleanUp() {
    supplyLineRepository.deleteAll();
    orderLineRepository.deleteAll();
    orderRepository.deleteAll();
    stockRepository.deleteAll();
    requisitionRepository.deleteAll();
    supervisoryNodeRepository.deleteAll();
    programRepository.deleteAll();
    periodRepository.deleteAll();
    scheduleRepository.deleteAll();
    productRepository.deleteAll();
    productCategoryRepository.deleteAll();
    Iterable<User> users = userRepository.findByUsername(USERNAME);
    if (users != null && users.iterator().hasNext()) {
      userRepository.delete(users);
    }
    facilityRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    stockInventoryRepository.deleteAll();
  }

  private Facility addFacility(String facilityName, String facilityCode, String facilityDescription,
                               FacilityType facilityType, GeographicZone geographicZone,
                               StockInventory stockInventory, boolean isActive, boolean isEnabled) {
    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(facilityCode);
    facility.setName(facilityName);
    facility.setDescription(facilityDescription);
    facility.setActive(isActive);
    facility.setEnabled(isEnabled);
    facility.setStockInventory(stockInventory);
    return facilityRepository.save(facility);
  }

  private Program addProgram(String programCode) {
    Program program = new Program();
    program.setCode(programCode);
    return programRepository.save(program);
  }

  private User addUser(String username, String password, String firstName, String lastName,
                       Facility facility) {
    User user = new User();
    user.setUsername(username);
    user.setPassword(password);
    user.setFirstName(firstName);
    user.setLastName(lastName);
    user.setHomeFacility(facility);
    return userRepository.save(user);
  }

  private Order addOrder(Requisition requisition, String orderCode, Program program, User user,
                         Facility requestingFacility, Facility receivingFacility,
                         Facility supplyingFacility, OrderStatus orderStatus, BigDecimal cost) {
    Order order = new Order();
    order.setRequisition(requisition);
    order.setOrderCode(orderCode);
    order.setQuotedCost(cost);
    order.setStatus(orderStatus);
    order.setProgram(program);
    order.setCreatedBy(user);
    order.setRequestingFacility(requestingFacility);
    order.setReceivingFacility(receivingFacility);
    order.setSupplyingFacility(supplyingFacility);
    return orderRepository.save(order);
  }

  private Product addProduct(String productName, String productCode, String dispensingUnit,
                             int dosesPerDispensingUnit, int packSize, int packRoundingThreshold,
                             boolean roundToZero, boolean isActive, boolean isFullSupply,
                             boolean isTraced, ProductCategory productCategory) {
    Product product = new Product();
    product.setPrimaryName(productName);
    product.setCode(productCode);
    product.setDispensingUnit(dispensingUnit);
    product.setDosesPerDispensingUnit(dosesPerDispensingUnit);
    product.setPackSize(packSize);
    product.setPackRoundingThreshold(packRoundingThreshold);
    product.setRoundToZero(roundToZero);
    product.setActive(isActive);
    product.setFullSupply(isFullSupply);
    product.setTracer(isTraced);
    product.setProductCategory(productCategory);
    return productRepository.save(product);
  }

  private Schedule addSchedule(String scheduleName, String scheduleCode) {
    Schedule schedule = new Schedule();
    schedule.setCode(scheduleCode);
    schedule.setName(scheduleName);
    return scheduleRepository.save(schedule);
  }

  private Period addPeriod(String periodName, Schedule processingSchedule,
                           LocalDate startDate, LocalDate endDate) {
    Period period = new Period();
    period.setProcessingSchedule(processingSchedule);
    period.setName(periodName);
    period.setStartDate(startDate);
    period.setEndDate(endDate);
    return periodRepository.save(period);
  }

  private Requisition addRequisition(Program program, Facility facility, Period processingPeriod,
                                     RequisitionStatus requisitionStatus,
                                     SupervisoryNode supervisoryNode) {
    Requisition requisition = new Requisition();
    requisition.setProgram(program);
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(processingPeriod);
    requisition.setStatus(requisitionStatus);
    requisition.setEmergency(false);
    requisition.setSupervisoryNode(supervisoryNode);
    return requisitionRepository.save(requisition);
  }

  private OrderLine addOrderLine(Order order, Product product, Long filledQuantity,
                                 Long orderedQuantity, String batch, LocalDate expiryDate,
                                 String vvm, String manufacturer) {
    OrderLine orderLine = new OrderLine();
    orderLine.setOrder(order);
    orderLine.setProduct(product);
    orderLine.setOrderedQuantity(orderedQuantity);
    orderLine.setFilledQuantity(filledQuantity);
    orderLine.setBatch(batch);
    orderLine.setExpiryDate(expiryDate);
    orderLine.setVvm(vvm);
    orderLine.setManufacturer(manufacturer);
    return orderLineRepository.save(orderLine);
  }

  private SupervisoryNode addSupervisoryNode(Facility supplyingFacility) {
    SupervisoryNode supervisoryNode = new SupervisoryNode();
    supervisoryNode.setCode("NodeCode");
    supervisoryNode.setName("NodeName");
    supervisoryNode.setFacility(supplyingFacility);
    return supervisoryNodeRepository.save(supervisoryNode);
  }

  private SupplyLine addSupplyLine(SupervisoryNode supervisoryNode, Program program,
                                   Facility supplyingFacility) {
    SupplyLine supplyLine = new SupplyLine();
    supplyLine.setSupervisoryNode(supervisoryNode);
    supplyLine.setProgram(program);
    supplyLine.setSupplyingFacility(supplyingFacility);
    return supplyLineRepository.save(supplyLine);
  }

  private Stock addStock(Product product, Long quantity) {
    Stock stock = new Stock();
    stock.setStockInventory(firstStockInventory);
    stock.setProduct(product);
    stock.setStoredQuantity(quantity);
    return stockRepository.save(stock);
  }

  private ProductCategory addProductCategory(String code, String name, int displayOrder) {
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCode(code);
    productCategory.setName(name);
    productCategory.setDisplayOrder(displayOrder);
    return productCategoryRepository.save(productCategory);
  }

  private GeographicLevel addGeographicLevel(String code, int level) {
    GeographicLevel geographicLevel = new GeographicLevel();
    geographicLevel.setCode(code);
    geographicLevel.setLevelNumber(level);
    return geographicLevelRepository.save(geographicLevel);
  }

  private GeographicZone addGeographicZone(String code, GeographicLevel geographicLevel) {
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(code);
    geographicZone.setLevel(geographicLevel);
    return geographicZoneRepository.save(geographicZone);
  }

  private FacilityType addFacilityType(String code) {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode(code);
    return facilityTypeRepository.save(facilityType);
  }

  @Test
  public void testCorrectOrderToFinalize() throws JsonProcessingException {
    addOrderLine(firstOrder, firstProduct, 100L, 123L, "orderLineBatchNumber1",
                 LocalDate.of(2016, 1, 1), "orderLineVvm1", "orderLineManufacturer1");
    addStock(firstProduct, 1234L);
    addOrderLine(firstOrder, secondProduct, 12345L, 12345L, "orderLineBatchNumber2",
                 LocalDate.of(2016, 1, 1), "orderLineVvm2", "orderLineManufacturer2");
    addStock(secondProduct, 123456L);

    HttpHeaders headers = new HttpHeaders();
    RestTemplate restTemplate = new RestTemplate();

    UriComponents uriComponents = UriComponentsBuilder.fromHttpUrl(addTokenToUrl(
        RESOURCE_FINALIZE_URL)).build().expand(firstOrder.getId().toString()).encode();
    String uri = uriComponents.toUriString();
    HttpEntity<String> entity = new HttpEntity<>(headers);
    ResponseEntity<?> result =  restTemplate.exchange(uri, HttpMethod.PUT, entity, String.class);

    Assert.assertEquals(result.getStatusCode(), HttpStatus.OK);

    Order resultOrder = orderRepository.findOne(firstOrder.getId());
    Assert.assertEquals(resultOrder.getStatus(), OrderStatus.SHIPPED);

    Stock stock1 = stockRepository
        .findByStockInventoryAndProduct(firstStockInventory, firstProduct);
    Assert.assertEquals(stock1.getStoredQuantity().longValue(), 1111L);

    Stock stock2 = stockRepository
        .findByStockInventoryAndProduct(firstStockInventory, secondProduct);
    Assert.assertEquals(stock2.getStoredQuantity().longValue(), 111111L);
  }

  @Test(expected = HttpClientErrorException.class)
  public void testWhenStockDoesNotExist() throws JsonProcessingException {
    addOrderLine(firstOrder, firstProduct, 100L, 123L, "orderLineBatchNumber1",
            LocalDate.of(2016, 1, 1), "orderLineVvm1", "orderLineManufacturer1");
    addStock(firstProduct, 1234L);
    addOrderLine(firstOrder, secondProduct, 12345L, 12345L, "orderLineBatchNumber2",
            LocalDate.of(2016, 1, 1), "orderLineVvm2", "orderLineManufacturer2");

    HttpHeaders headers = new HttpHeaders();
    RestTemplate restTemplate = new RestTemplate();

    UriComponents uriComponents = UriComponentsBuilder.fromHttpUrl(addTokenToUrl(
        RESOURCE_FINALIZE_URL)).build().expand(firstOrder.getId().toString()).encode();
    String uri = uriComponents.toUriString();
    HttpEntity<String> entity = new HttpEntity<>(headers);


    restTemplate.exchange(uri, HttpMethod.PUT, entity, String.class);
  }

  @Test(expected = HttpClientErrorException.class)
  public void testStockWithInsufficientQuantity() throws JsonProcessingException {
    addOrderLine(firstOrder, firstProduct, 100L, 123L, "orderLineBatchNumber1",
            LocalDate.of(2016, 1, 1), "orderLineVvm1", "orderLineManufacturer1");
    addStock(firstProduct, 1234L);
    addOrderLine(firstOrder, secondProduct, 12345L, 12345L, "orderLineBatchNumber2",
            LocalDate.of(2016, 1, 1), "orderLineVvm2", "orderLineManufacturer2");
    addStock(secondProduct, 12L);

    HttpHeaders headers = new HttpHeaders();
    RestTemplate restTemplate = new RestTemplate();

    UriComponents uriComponents = UriComponentsBuilder.fromHttpUrl(addTokenToUrl(
        RESOURCE_FINALIZE_URL)).build().expand(firstOrder.getId().toString()).encode();
    String uri = uriComponents.toUriString();
    HttpEntity<String> entity = new HttpEntity<>(headers);

    restTemplate.exchange(uri, HttpMethod.PUT, entity, String.class);
  }

  @Test(expected = HttpClientErrorException.class)
  public void testWrongOrderStatus() throws JsonProcessingException {
    firstOrder.setStatus(OrderStatus.SHIPPED);
    orderRepository.save(firstOrder);

    HttpHeaders headers = new HttpHeaders();
    RestTemplate restTemplate = new RestTemplate();

    UriComponents uriComponents = UriComponentsBuilder.fromHttpUrl(addTokenToUrl(
        RESOURCE_FINALIZE_URL)).build().expand(firstOrder.getId().toString()).encode();
    String uri = uriComponents.toUriString();
    HttpEntity<String> entity = new HttpEntity<>(headers);

    restTemplate.exchange(uri, HttpMethod.PUT, entity, String.class);
  }

  @Test
  public void testPrintOrderAsCsv() {
    RestTemplate restTemplate = new RestTemplate();

    UriComponentsBuilder builder = UriComponentsBuilder.fromHttpUrl(RESOURCE_URL + "/"
            + secondOrder.getId() + "/print")
            .queryParam("format", "csv")
            .queryParam("access_token", getToken());

    ResponseEntity<?> printOrderResponse = restTemplate.exchange(builder.toUriString(),
            HttpMethod.GET,
            null,
            new ParameterizedTypeReference<String>() { });

    String csvContent = printOrderResponse.getBody().toString();
    Assert.assertTrue(csvContent.startsWith("productName,filledQuantity,orderedQuantity"));
    for (OrderLine o : orderRepository.findOne(secondOrder.getId()).getOrderLines()) {
      Assert.assertTrue(csvContent.contains(o.getProduct().getPrimaryName()
              + "," + o.getFilledQuantity()
              + "," + o.getOrderedQuantity()));
    }
  }

  @Test
  public void testPrintOrderAsPdf() {
    RestTemplate restTemplate = new RestTemplate();

    UriComponentsBuilder builder = UriComponentsBuilder.fromHttpUrl(RESOURCE_URL + "/"
            + thirdOrder.getId() + "/print")
            .queryParam("format", "pdf")
            .queryParam("access_token", getToken());

    ResponseEntity<?> printOrderResponse = restTemplate.exchange(builder.toUriString(),
            HttpMethod.GET,
            null,
            new ParameterizedTypeReference<String>() { });

    String pdfContent = printOrderResponse.getBody().toString();
    Assert.assertNotNull(pdfContent);
  }

  @Test
  public void testConvertToOrder() {
    RestTemplate restTemplate = new RestTemplate();
    String url = addTokenToUrl(RESOURCE_URL);

    orderRepository.deleteAll();

    restTemplate.exchange(url, HttpMethod.POST,
            new HttpEntity<Object>(Collections.singletonList(requisition)), String.class);

    Assert.assertEquals(1, orderRepository.count());
    Order order = orderRepository.findAll().iterator().next();

    Assert.assertEquals(user.getId(), order.getCreatedBy().getId());

    Assert.assertEquals(OrderStatus.ORDERED, order.getStatus());
    Assert.assertEquals(order.getRequisition().getId(), requisition.getId());
    Assert.assertEquals(order.getReceivingFacility().getId(), requisition.getFacility().getId());
    Assert.assertEquals(order.getRequestingFacility().getId(), requisition.getFacility().getId());

    Assert.assertEquals(order.getProgram().getId(), requisition.getProgram().getId());
    Assert.assertEquals(order.getSupplyingFacility().getId(),
            supplyLine.getSupplyingFacility().getId());
  }
}
