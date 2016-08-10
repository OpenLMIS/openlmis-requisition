package org.openlmis.referencedata.service;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
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
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.service.RequisitionLineService;
import org.openlmis.requisition.service.RequisitionTemplateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
public class RequisitionLineServiceTest {

  private static final String REQUISITION_REPOSITORY_NAME = "RequisitionLineServiceIntegrationTest";
  private static final String BEGINNING_BALANCE_FIELD = "beginningBalance";
  private static final String TOTAL_QUANTITY_RECEIVED_FIELD = "totalQuantityReceived";
  private static final SourceType SOURCE = SourceType.CALCULATED;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private RequisitionLineRepository requisitionLineRepository;

  @Autowired
  private RequisitionLineService requisitionLineService;

  @Autowired
  private RequisitionTemplateService requisitionTemplateService;

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
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  private Requisition requisition = new Requisition();

  private Requisition secondRequisition = new Requisition();

  private Program program;

  private Product product;

  @Before
  public void setUp() {
    createTestRequisition();
  }

  @Test
  public void shouldInitiateBeginningBalance() {
    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();
    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD,
        new RequisitionTemplateColumn(BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, 1,
            true, false, true, true, SOURCE));
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setProgram(program);
    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);
    requisitionTemplateRepository.save(requisitionTemplate);

    requisitionLineService.initiateRequisitionLineFields(secondRequisition);

    RequisitionLine requisitionLine = secondRequisition.getRequisitionLines().iterator().next();

    Assert.assertEquals(20, requisitionLine.getBeginningBalance().intValue());
  }

  @Test
  public void shouldResetBeginningBalance() throws RequisitionException {
    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();
    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD,
        new RequisitionTemplateColumn(BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, 1,
            true, true, true, false, SOURCE));
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setProgram(program);
    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);
    requisitionTemplateRepository.save(requisitionTemplate);

    RequisitionLine requisitionLine = secondRequisition.getRequisitionLines().iterator().next();
    requisitionLine.setBeginningBalance(222);
    requisitionLineService.save(requisition, requisitionLine);

    Assert.assertEquals(20, requisitionLine.getBeginningBalance().intValue());
  }

  @Test
  public void shouldNotInitiateBeginningBalanceWhenItIsNotDisplayed() {
    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();
    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD,
        new RequisitionTemplateColumn(BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, 1,
            false, false, true, true, SOURCE));
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setProgram(program);
    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);
    requisitionTemplateRepository.save(requisitionTemplate);

    requisitionLineService.initiateRequisitionLineFields(secondRequisition);

    RequisitionLine requisitionLine = secondRequisition.getRequisitionLines().iterator().next();

    Assert.assertEquals(0, requisitionLine.getBeginningBalance().intValue());
  }

  @Test
  public void shouldDisplayColumnsInCorrectOrder() {
    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();
    requisitionTemplateColumnHashMap.put(BEGINNING_BALANCE_FIELD,
            new RequisitionTemplateColumn(BEGINNING_BALANCE_FIELD, BEGINNING_BALANCE_FIELD, 2,
                    false, false, true, true, SourceType.USER_INPUT));
    requisitionTemplateColumnHashMap.put(TOTAL_QUANTITY_RECEIVED_FIELD,
            new RequisitionTemplateColumn(TOTAL_QUANTITY_RECEIVED_FIELD,
                    TOTAL_QUANTITY_RECEIVED_FIELD, 1, false, false, true, true,
                    SourceType.USER_INPUT));
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setProgram(program);
    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);
    requisitionTemplateRepository.save(requisitionTemplate);

    requisitionLineService.initiateRequisitionLineFields(secondRequisition);

    List<RequisitionTemplate> requisitionTemplateList
        = requisitionTemplateService.searchRequisitionTemplates(secondRequisition.getProgram());
    Assert.assertEquals(1 ,requisitionTemplateList.size());
    Map<String, RequisitionTemplateColumn> testRequisitionTemplateColumnHashMap
        = requisitionTemplateList.get(0).getColumnsMap();

    Assert.assertEquals(1, testRequisitionTemplateColumnHashMap.get(TOTAL_QUANTITY_RECEIVED_FIELD)
            .getDisplayOrder());

    Assert.assertEquals(2, testRequisitionTemplateColumnHashMap.get(BEGINNING_BALANCE_FIELD)
            .getDisplayOrder());
  }

  @Test
  public void testSearchRequisitionLines() {
    requisitionLineRepository.deleteAll();
    RequisitionLine requisitionLine = createTestRequisitionLine(product, 10, 20, requisition);
    requisitionLineRepository.save(requisitionLine);

    List<RequisitionLine> receivedRequisitionLines = requisitionLineService.searchRequisitionLines(
        requisitionLine.getRequisition(), null);
    Assert.assertEquals(1, receivedRequisitionLines.size());

    Requisition expectedRequisition = requisitionLine.getRequisition();
    Requisition receivedRequisition = receivedRequisitionLines.get(0).getRequisition();

    Assert.assertEquals(expectedRequisition.getId(), receivedRequisition.getId());
  }

  private void createTestRequisition() {
    ProductCategory productCategory = new ProductCategory("code", "name", 1);
    productCategoryRepository.save(productCategory);

    product = new Product();
    product.setCode(REQUISITION_REPOSITORY_NAME);
    product.setPrimaryName(REQUISITION_REPOSITORY_NAME);
    product.setDispensingUnit(REQUISITION_REPOSITORY_NAME);
    product.setDosesPerDispensingUnit(10);
    product.setPackSize(1);
    product.setPackRoundingThreshold(0);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(true);
    product.setTracer(false);
    product.setProductCategory(productCategory);
    productRepository.save(product);

    program = new Program();
    program.setCode(REQUISITION_REPOSITORY_NAME);
    program.setPeriodsSkippable(true);
    programRepository.save(program);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode(REQUISITION_REPOSITORY_NAME);
    facilityTypeRepository.save(facilityType);
    GeographicLevel level = new GeographicLevel();
    level.setCode(REQUISITION_REPOSITORY_NAME);
    level.setLevelNumber(1);
    geographicLevelRepository.save(level);
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(REQUISITION_REPOSITORY_NAME);
    geographicZone.setLevel(level);
    geographicZoneRepository.save(geographicZone);


    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(REQUISITION_REPOSITORY_NAME);
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    Schedule schedule = new Schedule();
    schedule.setCode("code");
    schedule.setName("name");
    scheduleRepository.save(schedule);

    Period period = createTestPeriod("description", "name1", schedule,
        LocalDate.of(2016, 1 , 1), LocalDate.of(2016, 2, 1));
    Period secondPeriod = createTestPeriod("description", "name2", schedule,
        LocalDate.of(2016, 2 , 1), LocalDate.of(2016, 3, 1));
    periodRepository.save(period);
    periodRepository.save(secondPeriod);

    requisition = createTestRequisition(facility, period, program,
        RequisitionStatus.INITIATED);
    secondRequisition = createTestRequisition(facility, secondPeriod, program,
        RequisitionStatus.INITIATED);
    requisitionRepository.save(secondRequisition);
    requisitionRepository.save(requisition);

    RequisitionLine requisitionLine = createTestRequisitionLine(product, 10, 20, requisition);
    RequisitionLine secondRequisitionLine = createTestRequisitionLine(
        product, 100, 50, secondRequisition);
    requisitionLineRepository.save(requisitionLine);
    requisitionLineRepository.save(secondRequisitionLine);

    requisition.setRequisitionLines(new HashSet<>(Arrays.asList(requisitionLine)));
    secondRequisition.setRequisitionLines(new HashSet<>(Arrays.asList(secondRequisitionLine)));
    requisitionRepository.save(requisition);
    requisitionRepository.save(secondRequisition);

  }

  private Requisition createTestRequisition(
      Facility facility, Period period, Program program, RequisitionStatus requisitionStatus) {
    Requisition requisition = new Requisition();
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(period);
    requisition.setProgram(program);
    requisition.setStatus(requisitionStatus);
    return requisition;
  }

  private RequisitionLine createTestRequisitionLine(
      Product product, Integer quantityRequested, Integer stockInHand, Requisition requisition) {
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setProduct(product);
    requisitionLine.setRequestedQuantity(quantityRequested);
    requisitionLine.setStockInHand(stockInHand);
    requisitionLine.setRequisition(requisition);
    return requisitionLine;
  }

  private Period createTestPeriod(String description, String name,
                                  Schedule schedule, LocalDate startDate, LocalDate endDate) {
    Period period = new Period();
    period.setDescription(description);
    period.setName(name);
    period.setProcessingSchedule(schedule);
    period.setStartDate(startDate);
    period.setEndDate(endDate);
    return period;
  }

}


