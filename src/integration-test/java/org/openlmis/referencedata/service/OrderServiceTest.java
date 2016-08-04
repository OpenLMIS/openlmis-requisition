package org.openlmis.referencedata.service;

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
import org.openlmis.fulfillment.service.OrderService;
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
import org.openlmis.referencedata.domain.SupplyLine;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.openlmis.referencedata.repository.GeographicLevelRepository;
import org.openlmis.referencedata.repository.GeographicZoneRepository;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ProgramRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.openlmis.referencedata.repository.SupplyLineRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
public class OrderServiceTest {

  private static final Integer REQUESTED_QUANTITY = 10;

  @Autowired
  private OrderService orderService;

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private OrderLineRepository orderLineRepository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private SupervisoryNodeRepository supervisoryNodeRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private RequisitionLineRepository requisitionLineRepository;

  @Autowired
  private UserRepository userRepository;

  @Autowired
  private SupplyLineRepository supplyLineRepository;

  private List<Requisition> requisitionList = new ArrayList<>();
  private SupplyLine supplyLine = new SupplyLine();
  private User user;

  @Before
  public void setUp() {
    Assert.assertEquals(1, userRepository.count());
    user = userRepository.findAll().iterator().next();

    Program program = new Program();
    program.setCode("programCode");
    program.setPeriodsSkippable(true);
    programRepository.save(program);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode("TypeCode");
    facilityTypeRepository.save(facilityType);

    GeographicLevel level = new GeographicLevel();
    level.setCode("LevelCode");
    level.setLevelNumber(1);
    geographicLevelRepository.save(level);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("ZoneCode");
    geographicZone.setLevel(level);
    geographicZoneRepository.save(geographicZone);

    Facility supplyingFacility = new Facility();
    supplyingFacility.setType(facilityType);
    supplyingFacility.setGeographicZone(geographicZone);
    supplyingFacility.setCode("FacilityCode");
    supplyingFacility.setActive(true);
    supplyingFacility.setEnabled(true);
    facilityRepository.save(supplyingFacility);

    SupervisoryNode supervisoryNode = new SupervisoryNode();
    supervisoryNode.setCode("NodeCode");
    supervisoryNode.setName("NodeName");
    supervisoryNode.setFacility(supplyingFacility);
    supervisoryNodeRepository.save(supervisoryNode);

    supplyLine.setSupervisoryNode(supervisoryNode);
    supplyLine.setProgram(program);
    supplyLine.setSupplyingFacility(supplyingFacility);
    supplyLineRepository.save(supplyLine);

    requisitionList.add(createTestRequisition("code1", program, supervisoryNode));
    requisitionList.add(createTestRequisition("code2", program, supervisoryNode));
  }

  @Test
  public void shouldConvertToOrder() {
    Assert.assertEquals(0, orderRepository.count());
    orderService.convertToOrder(requisitionList, user.getId());

    Assert.assertEquals(2, orderRepository.count());
    Order order = orderRepository.findAll().iterator().next();

    Requisition requisition = requisitionList.get(0);
    if (!requisition.getId().equals(order.getRequisition().getId())) {
      requisition = requisitionList.get(1);
    }

    requisition = requisitionRepository.findOne(requisition.getId());

    Assert.assertEquals(OrderStatus.ORDERED, order.getStatus());
    Assert.assertEquals(order.getRequisition().getId(), requisition.getId());
    Assert.assertEquals(order.getReceivingFacility().getId(), requisition.getFacility().getId());
    Assert.assertEquals(order.getRequestingFacility().getId(), requisition.getFacility().getId());

    Assert.assertEquals(order.getProgram().getId(), requisition.getProgram().getId());
    Assert.assertEquals(order.getSupplyingFacility().getId(),
        supplyLine.getSupplyingFacility().getId());

    Assert.assertEquals(1, order.getOrderLines().size());
    Assert.assertEquals(1, requisition.getRequisitionLines().size());

    OrderLine orderLine = order.getOrderLines().iterator().next();
    RequisitionLine requisitionLine = requisition.getRequisitionLines().iterator().next();

    Assert.assertEquals(requisitionLine.getRequestedQuantity().longValue(),
        orderLine.getOrderedQuantity().longValue());
    Assert.assertEquals(requisitionLine.getProduct().getId(), orderLine.getProduct().getId());
  }

  private Requisition createTestRequisition(String code, Program program,
                                            SupervisoryNode supervisoryNode) {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode(code);
    facilityTypeRepository.save(facilityType);

    GeographicLevel level = new GeographicLevel();
    level.setCode(code);
    level.setLevelNumber(1);
    geographicLevelRepository.save(level);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(code);
    geographicZone.setLevel(level);
    geographicZoneRepository.save(geographicZone);

    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(code);
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    Schedule schedule = new Schedule();
    schedule.setCode(code);
    schedule.setName(code);
    scheduleRepository.save(schedule);

    Period period = new Period();
    period.setProcessingSchedule(schedule);
    period.setName(code);
    period.setStartDate(LocalDate.of(2016, 7, 10));
    period.setEndDate(LocalDate.of(2016, 7, 15));
    periodRepository.save(period);

    Requisition requisition = new Requisition();
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(period);
    requisition.setProgram(program);
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setSupervisoryNode(supervisoryNode);
    requisition.setEmergency(false);
    requisitionRepository.save(requisition);

    ProductCategory productCategory = new ProductCategory(code, code, 1);
    productCategoryRepository.save(productCategory);

    Product product = new Product(code, "primaryName", "dispensingUnit",
        1, 1, 1, true, true, true, true, productCategory);
    productRepository.save(product);

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequisition(requisition);
    requisitionLine.setRequestedQuantity(REQUESTED_QUANTITY);
    requisitionLine.setProduct(product);
    requisitionLineRepository.save(requisitionLine);

    return requisition;
  }
}
