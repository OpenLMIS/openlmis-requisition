package org.openlmis.referencedata.web;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.product.domain.Product;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Stock;
import org.openlmis.referencedata.domain.StockInventory;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.openlmis.referencedata.repository.GeographicLevelRepository;
import org.openlmis.referencedata.repository.GeographicZoneRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.repository.StockInventoryRepository;
import org.openlmis.referencedata.repository.StockRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import java.math.BigDecimal;
import java.time.LocalDate;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@WebIntegrationTest("server.port:8080")
public class OrderControllerIntegrationTest {

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

  private static final String RESOURCE_URL = "http://localhost:8080/api/orders/finalizeOrder";

  private Order order = new Order();
  private Product firstProduct = new Product();
  private Product secondProduct = new Product();
  private StockInventory stockInventory = new StockInventory();

  @Before
  public void setUp() {
    cleanUp();

    stockInventory.setName("stockInventoryName");
    stockInventoryRepository.save(stockInventory);

    GeographicLevel geographicLevel = new GeographicLevel();
    geographicLevel.setCode("geographicLevelCode");
    geographicLevel.setLevelNumber(1);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode("facilityTypeCode");

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("geographicZoneCode");
    geographicZone.setLevel(geographicLevel);

    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("facilityCode");
    facility.setName("facilityName");
    facility.setDescription("facilityDescription");
    facility.setActive(true);
    facility.setEnabled(true);
    facility.setStockInventory(stockInventory);
    facilityRepository.save(facility);

    Program program = new Program();
    program.setCode("programCode");
    programRepository.save(program);

    User user = new User();
    user.setUsername("userName");
    user.setPassword("userPassword");
    user.setFirstName("userFirstName");
    user.setLastName("userLastName");
    userRepository.save(user);

    order.setOrderCode("orderCode");
    order.setQuotedCost(new BigDecimal("1.29"));
    order.setStatus(OrderStatus.PICKING);
    order.setProgram(program);
    order.setCreatedBy(user);
    order.setRequestingFacility(facility);
    order.setReceivingFacility(facility);
    order.setSupplyingFacility(facility);
    orderRepository.save(order);

    firstProduct.setPrimaryName("firstProductName");
    firstProduct.setCode("firstProductCode");
    firstProduct.setDispensingUnit("unit");
    firstProduct.setDosesPerDispensingUnit(10);
    firstProduct.setPackSize(1);
    firstProduct.setPackRoundingThreshold(0);
    firstProduct.setRoundToZero(false);
    firstProduct.setActive(true);
    firstProduct.setFullSupply(true);
    firstProduct.setTracer(false);
    productRepository.save(firstProduct);

    secondProduct.setPrimaryName("secondProductName");
    secondProduct.setCode("secondProductCode");
    secondProduct.setDispensingUnit("unit");
    secondProduct.setDosesPerDispensingUnit(10);
    secondProduct.setPackSize(1);
    secondProduct.setPackRoundingThreshold(0);
    secondProduct.setRoundToZero(false);
    secondProduct.setActive(true);
    secondProduct.setFullSupply(true);
    secondProduct.setTracer(false);
    productRepository.save(secondProduct);
  }

  @After
  public void cleanUp(){
    stockRepository.deleteAll();
    orderLineRepository.deleteAll();
    productRepository.deleteAll();
    orderRepository.deleteAll();
    userRepository.deleteAll();
    programRepository.deleteAll();
    facilityRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    stockInventoryRepository.deleteAll();
  }

  private void addOrderLine(Product product, Long quantity) {
    OrderLine orderLine = new OrderLine();
    orderLine.setOrder(order);
    orderLine.setProduct(product);
    orderLine.setOrderedQuantity(quantity);
    orderLine.setBatch("orderLineBatchNumber");
    orderLine.setExpiryDate(LocalDate.of(2016, 1, 1));
    orderLine.setVvm("orderLieVvm");
    orderLine.setManufacturer("orderLineManufacturer");
    orderLineRepository.save(orderLine);
  }

  private void addStock(Product product, Long quantity) {
    Stock stock = new Stock();
    stock.setStockInventory(stockInventory);
    stock.setProduct(product);
    stock.setStoredQuantity(quantity);
    stockRepository.save(stock);
  }

  @Test
  public void testCorrectOrderToFinalize() throws JsonProcessingException {
    addOrderLine(firstProduct, 123L);
    addStock(firstProduct, 1234L);
    addOrderLine(secondProduct, 12345L);
    addStock(secondProduct, 123456L);

    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    RestTemplate restTemplate = new RestTemplate();

    ObjectMapper mapper = new ObjectMapper();
    String orderJson = mapper.writeValueAsString(order.getId());
    HttpEntity<String> entity = new HttpEntity<>(orderJson, headers);

    ResponseEntity<?> result=restTemplate.postForEntity(RESOURCE_URL, entity, String.class);

    Assert.assertEquals(result.getStatusCode(), HttpStatus.OK);
  }

  @Test(expected = HttpClientErrorException.class)
  public void testWhenStockDoesNotExist() throws JsonProcessingException {
    addOrderLine(firstProduct, 123L);
    addStock(firstProduct, 1234L);
    addOrderLine(secondProduct, 12345L);

    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    RestTemplate restTemplate = new RestTemplate();

    ObjectMapper mapper = new ObjectMapper();
    String orderJson = mapper.writeValueAsString(order.getId());
    HttpEntity<String> entity = new HttpEntity<>(orderJson, headers);

    restTemplate.postForEntity(RESOURCE_URL, entity, String.class);
  }

  @Test(expected = HttpClientErrorException.class)
  public void testStockWithInsufficientQuantity() throws JsonProcessingException {
    addOrderLine(firstProduct, 123L);
    addStock(firstProduct, 1234L);
    addOrderLine(secondProduct, 12345L);
    addStock(secondProduct, 12L);

    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    RestTemplate restTemplate = new RestTemplate();

    ObjectMapper mapper = new ObjectMapper();
    String orderJson = mapper.writeValueAsString(order.getId());
    HttpEntity<String> entity = new HttpEntity<>(orderJson, headers);

    restTemplate.postForEntity(RESOURCE_URL, entity, String.class);
  }
}
