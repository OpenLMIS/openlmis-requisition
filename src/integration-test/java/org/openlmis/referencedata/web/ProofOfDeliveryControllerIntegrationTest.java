package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.openlmis.fulfillment.domain.ProofOfDeliveryLine;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryLineRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
import org.openlmis.hierarchyandsupervision.domain.User;
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
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.openlmis.referencedata.repository.GeographicLevelRepository;
import org.openlmis.referencedata.repository.GeographicZoneRepository;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.Assert.assertThat;

public class ProofOfDeliveryControllerIntegrationTest extends BaseWebIntegrationTest {

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private OrderLineRepository orderLineRepository;

  @Autowired
  private UserRepository userRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  @Autowired
  private ProofOfDeliveryRepository proofOfDeliveryRepository;

  @Autowired
  private ProofOfDeliveryLineRepository proofOfDeliveryLineRepository;

  private ProofOfDelivery proofOfDelivery =
      new ProofOfDelivery();
  private ProofOfDeliveryLine proofOfDeliveryLine1 =
      new ProofOfDeliveryLine();
  private ProofOfDeliveryLine proofOfDeliveryLine2 =
      new ProofOfDeliveryLine();
  private List<ProofOfDeliveryLine> proofOfDeliveryLineList =
      new ArrayList<ProofOfDeliveryLine>();

  @Before
  public void setUp() {
    cleanUp();

    Schedule schedule1 = new Schedule();
    schedule1.setCode("S1");
    schedule1.setName("Schedule1");
    scheduleRepository.save(schedule1);

    Program program1 = new Program();
    program1.setCode("P1");
    program1.setName("Program1");
    programRepository.save(program1);

    Period period1 = new Period();
    period1.setProcessingSchedule(schedule1);
    period1.setName("P1");
    period1.setStartDate(LocalDate.of(2015, Month.JANUARY, 1));
    period1.setEndDate(LocalDate.of(2015, Month.DECEMBER, 31));
    periodRepository.save(period1);

    Facility facility1 = initFacility1();
    Requisition requisition1 = new Requisition();
    requisition1.setProgram(program1);
    requisition1.setCreatedDate(LocalDateTime.of(2015, Month.JANUARY, 1, 10, 0, 0));
    requisition1.setFacility(facility1);
    requisition1.setProcessingPeriod(period1);
    requisition1.setStatus(RequisitionStatus.RELEASED);
    requisitionRepository.save(requisition1);

    Assert.assertEquals(1, userRepository.count());
    User user = userRepository.findAll().iterator().next();

    OrderLine orderLine1 = new OrderLine();
    OrderLine orderLine2 = new OrderLine();
    Product product1 = initProduct1();
    Product product2 = initProduct2();
    Facility facility2 = initFacility2();
    Order order1 = initOrder(user, program1, facility1, facility2, product1, product2,
        orderLine1, orderLine2);

    proofOfDelivery.setOrder(order1);
    proofOfDelivery.setTotalShippedPacks(100);
    proofOfDelivery.setTotalReceivedPacks(100);
    proofOfDelivery.setTotalReturnedPacks(10);
    proofOfDelivery.setDeliveredBy("delivered by");
    proofOfDelivery.setReceivedBy("received by");
    proofOfDelivery.setReceivedDate(new Date());
    proofOfDeliveryRepository.save(proofOfDelivery);

    initProofOfDeliverLine1(orderLine1);
    initProofOfdeliverLine2(orderLine2);
    proofOfDeliveryLineList.add(proofOfDeliveryLine1);
    proofOfDeliveryLineList.add(proofOfDeliveryLine2);
    proofOfDelivery = proofOfDeliveryRepository.save(proofOfDelivery);
  }

  private void initProofOfdeliverLine2(OrderLine orderLine2) {
    proofOfDeliveryLine2.setOrderLine(orderLine2);
    proofOfDeliveryLine2.setProofOfDelivery(proofOfDelivery);
    proofOfDeliveryLine2.setQuantityShipped(200L);
    proofOfDeliveryLine2.setQuantityReturned(200L);
    proofOfDeliveryLine2.setQuantityReceived(200L);
    proofOfDeliveryLine2.setPackToShip(200L);
    proofOfDeliveryLine2.setReplacedProductCode("replaced product code2");
    proofOfDeliveryLine2.setNotes("Notes2");
    proofOfDeliveryLineRepository.save(proofOfDeliveryLine2);
  }

  private void initProofOfDeliverLine1(OrderLine orderLine1) {
    proofOfDeliveryLine1.setOrderLine(orderLine1);
    proofOfDeliveryLine1.setProofOfDelivery(proofOfDelivery);
    proofOfDeliveryLine1.setQuantityShipped(100L);
    proofOfDeliveryLine1.setQuantityReturned(100L);
    proofOfDeliveryLine1.setQuantityReceived(100L);
    proofOfDeliveryLine1.setPackToShip(100L);
    proofOfDeliveryLine1.setReplacedProductCode("replaced product code");
    proofOfDeliveryLine1.setNotes("Notes");
    proofOfDeliveryLineRepository.save(proofOfDeliveryLine1);
  }

  private Order initOrder(User user, Program program1, Facility facility1, Facility facility2,
                          Product product1, Product product2, OrderLine orderLine1,
                          OrderLine orderLine2) {
    Order order1 = new Order();
    order1.setStatus(OrderStatus.SHIPPED);
    order1.setCreatedDate(LocalDateTime.now());
    order1.setCreatedBy(user);
    order1.setOrderCode("O1");
    order1.setProgram(program1);
    order1.setQuotedCost(new BigDecimal(100));
    order1.setSupplyingFacility(facility1);
    order1.setRequestingFacility(facility2);
    order1.setReceivingFacility(facility2);
    orderRepository.save(order1);

    orderLine1.setOrder(order1);
    orderLine1.setProduct(product1);
    orderLine1.setOrderedQuantity(new Long(50));
    orderLine1.setFilledQuantity(50L);
    orderLineRepository.save(orderLine1);

    orderLine2.setOrder(order1);
    orderLine2.setProduct(product2);
    orderLine2.setOrderedQuantity(new Long(20));
    orderLine2.setFilledQuantity(15L);
    orderLineRepository.save(orderLine2);

    Set<OrderLine> orderLines = new HashSet<>();
    orderLines.add(orderLine1);
    orderLines.add(orderLine2);
    order1 = orderRepository.save(order1);
    return order1;
  }

  private Facility initFacility2() {
    GeographicLevel geographicLevel2 = new GeographicLevel();
    geographicLevel2.setCode("GL2");
    geographicLevel2.setLevelNumber(1);
    geographicLevelRepository.save(geographicLevel2);
    GeographicZone geographicZone2 = new GeographicZone();
    geographicZone2.setCode("GZ2");
    geographicZone2.setLevel(geographicLevel2);
    geographicZoneRepository.save(geographicZone2);
    FacilityType facilityType2 = new FacilityType();
    facilityType2.setCode("FT2");
    facilityTypeRepository.save(facilityType2);
    Facility facility2 = new Facility();
    facility2.setCode("F2");
    facility2.setName("Facility2");
    facility2.setGeographicZone(geographicZone2);
    facility2.setType(facilityType2);
    facility2.setActive(true);
    facility2.setEnabled(false);
    facilityRepository.save(facility2);
    return facility2;
  }

  private Facility initFacility1() {
    GeographicLevel geographicLevel1 = new GeographicLevel();
    geographicLevel1.setCode("GL1");
    geographicLevel1.setLevelNumber(1);
    geographicLevelRepository.save(geographicLevel1);
    GeographicZone geographicZone1 = new GeographicZone();
    geographicZone1.setCode("GZ1");
    geographicZone1.setLevel(geographicLevel1);
    geographicZoneRepository.save(geographicZone1);
    FacilityType facilityType1 = new FacilityType();
    facilityType1.setCode("FT1");
    facilityTypeRepository.save(facilityType1);
    Facility facility1 = new Facility();
    facility1.setCode("F1");
    facility1.setName("Facility1");
    facility1.setGeographicZone(geographicZone1);
    facility1.setType(facilityType1);
    facility1.setActive(true);
    facility1.setEnabled(false);
    facilityRepository.save(facility1);
    return facility1;
  }

  private Product initProduct2() {
    ProductCategory productCategory2 = new ProductCategory();
    productCategory2.setCode("PC2");
    productCategory2.setName("PC2 name");
    productCategory2.setDisplayOrder(2);
    productCategoryRepository.save(productCategory2);

    Product product2 = new Product();
    product2.setCode("P2");
    product2.setPrimaryName("Product2");
    product2.setDispensingUnit("pills");
    product2.setDosesPerDispensingUnit(2);
    product2.setPackSize(20);
    product2.setPackRoundingThreshold(20);
    product2.setRoundToZero(true);
    product2.setActive(true);
    product2.setFullSupply(false);
    product2.setTracer(false);
    product2.setProductCategory(productCategory2);
    productRepository.save(product2);
    return product2;
  }

  private Product initProduct1() {
    ProductCategory productCategory1 = new ProductCategory();
    productCategory1.setCode("PC1");
    productCategory1.setName("PC1 name");
    productCategory1.setDisplayOrder(1);
    productCategoryRepository.save(productCategory1);

    Product product1 = new Product();
    product1.setCode("P1");
    product1.setPrimaryName("Product1");
    product1.setDispensingUnit("pills");
    product1.setDosesPerDispensingUnit(1);
    product1.setPackSize(10);
    product1.setPackRoundingThreshold(10);
    product1.setRoundToZero(false);
    product1.setActive(true);
    product1.setFullSupply(true);
    product1.setTracer(false);
    product1.setProductCategory(productCategory1);
    productRepository.save(product1);
    return product1;
  }

  /**
   * Cleanup the test environment.
   */
  @After
  public void cleanUp() {
    proofOfDeliveryLineRepository.deleteAll();
    proofOfDeliveryRepository.deleteAll();
    orderLineRepository.deleteAll();
    orderRepository.deleteAll();
    productRepository.deleteAll();
    productCategoryRepository.deleteAll();
    requisitionRepository.deleteAll();
    facilityRepository.deleteAll();
    programRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    geographicLevelRepository.deleteAll();
  }

  @Test
  public void testPrintProofOfDeliveryToPdf() {
    restAssured.given()
        .pathParam("id", proofOfDelivery.getId())
        .queryParam("access_token", getToken())
        .when()
        .get("/api/proofOfDeliveries/{id}/print")
        .then()
        .statusCode(200);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
