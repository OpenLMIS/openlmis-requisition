package org.openlmis.referencedata.service;

import org.junit.After;
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
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.service.RequisitionLineService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
public class RequisitionLineServiceTest {

  private static final String requisitionRepositoryName = "RequisitionLineServiceIntegrationTest";
  private static final String beginningBalanceField = "beginningBalance";

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private RequisitionLineRepository requisitionLineRepository;

  @Autowired
  private RequisitionLineService requisitionLineService;

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

  @Before
  public void setUp() {
    requisitionRepository.deleteAll();
    requisitionLineRepository.deleteAll();
    requisitionTemplateRepository.deleteAll();
    programRepository.deleteAll();
    periodRepository.deleteAll();
    facilityRepository.deleteAll();
    scheduleRepository.deleteAll();
    productRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    productCategoryRepository.deleteAll();

    createTestRequisition();
  }

  @After
  public void cleanUp() {
    requisitionRepository.deleteAll();
    requisitionLineRepository.deleteAll();
    requisitionTemplateRepository.deleteAll();
    periodRepository.deleteAll();
    facilityRepository.deleteAll();
    scheduleRepository.deleteAll();
    productRepository.deleteAll();
    programRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
  }

  @Test
  public void shouldInitiateBeginningBalance() {
    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();
    requisitionTemplateColumnHashMap.put(beginningBalanceField,
        new RequisitionTemplateColumn(beginningBalanceField, beginningBalanceField, 1,
            true, false, true, true, "source"));
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setProgram(program);
    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);
    requisitionTemplateRepository.save(requisitionTemplate);

    requisitionLineService.initiateRequisitionLineFields(secondRequisition);

    RequisitionLine requisitionLine = secondRequisition.getRequisitionLines().iterator().next();

    Assert.assertEquals(20, requisitionLine.getBeginningBalance().intValue());
  }

  @Test
  public void shouldResetBeginningBalance() {
    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();
    requisitionTemplateColumnHashMap.put(beginningBalanceField,
        new RequisitionTemplateColumn(beginningBalanceField, beginningBalanceField, 1,
            true, true, true, false, "source"));
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setProgram(program);
    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);
    requisitionTemplateRepository.save(requisitionTemplate);

    RequisitionLine requisitionLine = secondRequisition.getRequisitionLines().iterator().next();
    requisitionLine.setBeginningBalance(222);
    requisitionLineService.save(requisitionLine);

    Assert.assertEquals(20, requisitionLine.getBeginningBalance().intValue());
  }

  @Test
  public void shouldNotInitiateBeginningBalanceWhenItIsNotDisplayed() {
    HashMap<String, RequisitionTemplateColumn> requisitionTemplateColumnHashMap = new HashMap<>();
    requisitionTemplateColumnHashMap.put(beginningBalanceField,
        new RequisitionTemplateColumn(beginningBalanceField, beginningBalanceField, 1,
            false, false, true, true, "source"));
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setProgram(program);
    requisitionTemplate.setColumnsMap(requisitionTemplateColumnHashMap);
    requisitionTemplateRepository.save(requisitionTemplate);

    requisitionLineService.initiateRequisitionLineFields(secondRequisition);

    RequisitionLine requisitionLine = secondRequisition.getRequisitionLines().iterator().next();

    Assert.assertEquals(0, requisitionLine.getBeginningBalance().intValue());
  }

  private void createTestRequisition() {
    ProductCategory productCategory = new ProductCategory("code", "name", 1);
    productCategoryRepository.save(productCategory);

    Product product = new Product();
    product.setCode(requisitionRepositoryName);
    product.setPrimaryName(requisitionRepositoryName);
    product.setDispensingUnit(requisitionRepositoryName);
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
    program.setCode(requisitionRepositoryName);
    program.setPeriodsSkippable(true);
    programRepository.save(program);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode(requisitionRepositoryName);
    facilityTypeRepository.save(facilityType);
    GeographicLevel level = new GeographicLevel();
    level.setCode(requisitionRepositoryName);
    level.setLevelNumber(1);
    geographicLevelRepository.save(level);
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(requisitionRepositoryName);
    geographicZone.setLevel(level);
    geographicZoneRepository.save(geographicZone);


    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(requisitionRepositoryName);
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
    requisitionLine.setQuantityRequested(quantityRequested);
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


