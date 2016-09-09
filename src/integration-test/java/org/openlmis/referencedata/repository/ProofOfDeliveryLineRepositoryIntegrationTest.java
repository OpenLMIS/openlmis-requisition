package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;
import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.openlmis.fulfillment.domain.ProofOfDeliveryLine;
import org.openlmis.fulfillment.repository.OrderLineRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryLineRepository;
import org.openlmis.fulfillment.repository.ProofOfDeliveryRepository;
import org.openlmis.product.domain.Product;
import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.service.ReferenceDataService;
import org.openlmis.requisition.dto.UserDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
public class ProofOfDeliveryLineRepositoryIntegrationTest {

  @Autowired
  private OrderLineRepository orderLineRepository;

  @Autowired
  private ProofOfDeliveryRepository proofOfDeliveryRepository;

  @Autowired
  private ProofOfDeliveryLineRepository proofOfDeliveryLineRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private ReferenceDataService referenceDataService;

  private static final String CODE = "ProofOfDeliveryLineRepositoryIntegrationTest";

  private OrderLine orderLine = new OrderLine();

  private ProofOfDelivery proofOfDelivery = new ProofOfDelivery();

  @Before
  public void setUp() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode(CODE);
    facilityTypeRepository.save(facilityType);

    GeographicLevel level = new GeographicLevel();
    level.setCode(CODE);
    level.setLevelNumber(1);
    geographicLevelRepository.save(level);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(CODE);
    geographicZone.setLevel(level);
    geographicZoneRepository.save(geographicZone);

    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(CODE);
    facility.setName(CODE);
    facility.setDescription("Test facility");
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    Program program = new Program();
    program.setCode(CODE);
    programRepository.save(program);

    UserDto[] allUsers = referenceDataService.findAllUsers();
    Assert.assertEquals(1, allUsers.length);
    UserDto user = allUsers[0];

    Order order = new Order();
    order.setOrderCode(CODE);
    order.setQuotedCost(new BigDecimal("1.29"));
    order.setStatus(OrderStatus.PICKING);
    order.setProgram(program);
    order.setCreatedBy(user);
    order.setRequestingFacility(facility);
    order.setReceivingFacility(facility);
    order.setSupplyingFacility(facility);
    orderRepository.save(order);

    ProductCategory productCategory1 = new ProductCategory();
    productCategory1.setCode("PC1");
    productCategory1.setName("PC1 name");
    productCategory1.setDisplayOrder(1);
    productCategoryRepository.save(productCategory1);

    Product product = new Product();
    product.setCode(CODE);
    product.setPrimaryName("Product");
    product.setDispensingUnit("unit");
    product.setDosesPerDispensingUnit(10);
    product.setPackSize(1);
    product.setPackRoundingThreshold(0);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(true);
    product.setTracer(false);
    product.setProductCategory(productCategory1);
    productRepository.save(product);

    orderLine.setOrder(order);
    orderLine.setProduct(product);
    orderLine.setOrderedQuantity(5L);
    orderLine.setFilledQuantity(5L);
    orderLineRepository.save(orderLine);

    proofOfDelivery.setOrder(order);
    proofOfDelivery.setDeliveredBy(CODE);
    proofOfDelivery.setReceivedBy(CODE);
    proofOfDelivery.setReceivedDate(LocalDate.now());
    proofOfDeliveryRepository.save(proofOfDelivery);
  }

  @Test
  public void testCreate() {
    ProofOfDeliveryLine proofOfDeliveryLine = new ProofOfDeliveryLine();
    proofOfDeliveryLine.setOrderLine(orderLine);
    proofOfDeliveryLine.setProofOfDelivery(proofOfDelivery);

    Assert.assertNull(proofOfDeliveryLine.getId());

    proofOfDeliveryLine =
        proofOfDeliveryLineRepository.save(proofOfDeliveryLine);
    Assert.assertNotNull(proofOfDeliveryLine.getId());
  }
}
