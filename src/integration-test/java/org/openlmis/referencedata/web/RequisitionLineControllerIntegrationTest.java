package org.openlmis.referencedata.web;

import static org.junit.Assert.assertThat;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
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
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.LocalDate;

public class RequisitionLineControllerIntegrationTest extends BaseWebIntegrationTest {

  @Autowired
  private RequisitionLineRepository requisitionLineRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  private static final String RESOURCE_URL = BASE_URL + "/api/requisitionLines";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String ACCESS_TOKEN = "access_token";
  private static final String REQUISITION = "requisition";
  private static final String PRODUCT = "product";
  private static final String TEST_CODE = "123";
  private static final String TEST_NAME = "Name";
  private static final Integer BEGINNING_BALANCE = 100;
  private static final Integer TOTAL_RECEIVED_QUANTITY = 200;
  private static final Integer TOTAL_LOSSES_AND_ADJUSTMENTS = 300;

  private RequisitionLine requisitionLine;
  private Requisition requisition = new Requisition();
  private Period period = new Period();
  private Product product = new Product();
  private Program program = new Program();
  private Facility facility = new Facility();

  @Before
  public void setUp() {
    requisitionLine = generateRequisitionLine();
  }

  @After
  public void cleanUp() {
    requisitionLineRepository.deleteAll();
    productRepository.deleteAll();
    requisitionRepository.deleteAll();
    programRepository.deleteAll();
    periodRepository.deleteAll();
    facilityRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    periodRepository.deleteAll();
    scheduleRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    productCategoryRepository.deleteAll();
  }

  @Test
  public void testSearchRequisitionLines() {
    RequisitionLine[] response = restAssured.given()
        .queryParam(REQUISITION, requisition.getId())
        .queryParam(PRODUCT, product.getId())
        .queryParam(ACCESS_TOKEN, getToken())
        .when()
        .get(SEARCH_URL)
        .as(RequisitionLine[].class);

    assertThat(RAML_ASSERT_MESSAGE , restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    Assert.assertEquals(1,response.length);
    for ( RequisitionLine responseRequisitionLine : response ) {
      Assert.assertEquals(
          requisition.getId(),
          responseRequisitionLine.getRequisition().getId());
      Assert.assertEquals(
          product.getId(),
          responseRequisitionLine.getProduct().getId());
      Assert.assertEquals(
          BEGINNING_BALANCE,
          responseRequisitionLine.getBeginningBalance());
      Assert.assertEquals(
          TOTAL_RECEIVED_QUANTITY,
          responseRequisitionLine.getTotalReceivedQuantity());
      Assert.assertEquals(
          TOTAL_LOSSES_AND_ADJUSTMENTS,
          responseRequisitionLine.getTotalLossesAndAdjustments());
      Assert.assertEquals(
          requisitionLine.getId(),
          responseRequisitionLine.getId());
    }
  }

  private RequisitionLine generateRequisitionLine() {
    ProductCategory productCategory1 = new ProductCategory();
    productCategory1.setCode(TEST_CODE);
    productCategory1.setName(TEST_NAME);
    productCategory1.setDisplayOrder(1);
    productCategoryRepository.save(productCategory1);

    product.setCode(TEST_CODE);
    product.setPrimaryName(TEST_NAME);
    product.setDispensingUnit("Unit");
    product.setDosesPerDispensingUnit(10);
    product.setPackSize(1);
    product.setPackRoundingThreshold(0);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(true);
    product.setTracer(false);
    product.setProductCategory(productCategory1);
    product = productRepository.save(product);

    program.setCode(TEST_CODE);
    program.setPeriodsSkippable(true);
    programRepository.save(program);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode(TEST_CODE);
    facilityTypeRepository.save(facilityType);

    GeographicLevel level = new GeographicLevel();
    level.setCode(TEST_CODE);
    level.setLevelNumber(1);
    geographicLevelRepository.save(level);

    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(TEST_CODE);
    geographicZone.setLevel(level);
    geographicZoneRepository.save(geographicZone);

    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(TEST_CODE);
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    Schedule schedule = new Schedule();
    schedule.setCode(TEST_CODE);
    schedule.setName(TEST_NAME);
    scheduleRepository.save(schedule);

    period.setName(TEST_NAME);
    period.setProcessingSchedule(schedule);
    period.setDescription("Description");
    period.setStartDate(LocalDate.of(2016, 1, 1));
    period.setEndDate(LocalDate.of(2016, 2, 1));
    periodRepository.save(period);

    requisition.setFacility(facility);
    requisition.setProcessingPeriod(period);
    requisition.setProgram(program);
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition = requisitionRepository.save(requisition);

    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setProduct(product);
    requisitionLine.setRequisition(requisition);
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setBeginningBalance(BEGINNING_BALANCE);
    requisitionLine.setTotalReceivedQuantity(TOTAL_RECEIVED_QUANTITY);
    requisitionLine.setTotalLossesAndAdjustments(TOTAL_LOSSES_AND_ADJUSTMENTS);
    requisitionLineRepository.save(requisitionLine);

    return requisitionLine;
  }
}
