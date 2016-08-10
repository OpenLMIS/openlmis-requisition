package org.openlmis.referencedata.repository;

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
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionLineRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<RequisitionLine> {

  @Autowired
  private RequisitionLineRepository repository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  private List<RequisitionLine> requisitionLines;

  @Before
  public void setUp() {
    requisitionLines = new ArrayList<>();
    for (int requisitionLinesCount = 0; requisitionLinesCount < 5; requisitionLinesCount++) {
      requisitionLines.add(repository.save(generateInstance()));
    }
  }

  RequisitionLineRepository getRepository() {
    return this.repository;
  }

  RequisitionLine generateInstance() {
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setProduct(generateProduct());
    requisitionLine.setRequisition(generateRequisition());
    requisitionLine.setRequestedQuantity(1);
    requisitionLine.setStockOnHand(1);
    requisitionLine.setTotalConsumedQuantity(1);
    requisitionLine.setBeginningBalance(1);
    requisitionLine.setTotalReceivedQuantity(1);
    requisitionLine.setTotalLossesAndAdjustments(1);
    return requisitionLine;
  }

  @Test
  public void testSearchUsers() {
    List<RequisitionLine> receivedRequisitionLines = repository.searchRequisitionLines(
            requisitionLines.get(0).getRequisition(),
            requisitionLines.get(0).getProduct());

    Assert.assertEquals(1, receivedRequisitionLines.size());
    for (RequisitionLine requisitionLine : receivedRequisitionLines) {
      Assert.assertEquals(
              requisitionLine.getRequisition().getId(),
              requisitionLines.get(0).getRequisition().getId());
      Assert.assertEquals(
              requisitionLine.getProduct().getId(),
              requisitionLines.get(0).getProduct().getId());
    }
  }

  private Requisition generateRequisition() {
    Requisition requisition = new Requisition();
    requisition.setProgram(generateProgram());
    requisition.setFacility(generateFacility());
    requisition.setProcessingPeriod(generatePeriod());
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition);
    return requisition;
  }

  private Program generateProgram() {
    Program program = new Program();
    program.setCode("Program" + this.getNextInstanceNumber());
    program.setPeriodsSkippable(false);
    programRepository.save(program);
    return program;
  }

  private Period generatePeriod() {
    Period period = new Period();
    Integer instanceNumber = this.getNextInstanceNumber();
    period.setName("PeriodName" + instanceNumber);
    period.setDescription("PeriodDescription" + instanceNumber);
    period.setEndDate(LocalDate.now().plusDays(instanceNumber));
    period.setStartDate(LocalDate.now().minusDays(instanceNumber));
    period.setProcessingSchedule(generateSchedule());
    periodRepository.save(period);
    return period;
  }

  private Schedule generateSchedule() {
    Schedule schedule = new Schedule();
    schedule.setCode("Schedule" + this.getNextInstanceNumber());
    schedule.setName("name" + this.getNextInstanceNumber());
    scheduleRepository.save(schedule);
    return schedule;
  }

  private Facility generateFacility() {
    Integer instanceNumber = this.getNextInstanceNumber();
    GeographicZone geographicZone = generateGeographicZone();
    FacilityType facilityType = generateFacilityType();
    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("Facility" + instanceNumber);
    facility.setName("FacilityName" + instanceNumber);
    facility.setDescription("FacilityDescription" + instanceNumber);
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);
    return facility;
  }

  private GeographicLevel generateGeographicLevel() {
    GeographicLevel geographicLevel = new GeographicLevel();
    geographicLevel.setCode("GeographicLevel" + this.getNextInstanceNumber());
    geographicLevel.setLevelNumber(1);
    geographicLevelRepository.save(geographicLevel);
    return geographicLevel;
  }

  private GeographicZone generateGeographicZone() {
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("GeographicZone" + this.getNextInstanceNumber());
    geographicZone.setLevel(generateGeographicLevel());
    geographicZoneRepository.save(geographicZone);
    return geographicZone;
  }

  private FacilityType generateFacilityType() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode("FacilityType" + this.getNextInstanceNumber());
    facilityTypeRepository.save(facilityType);
    return facilityType;
  }

  private Product generateProduct() {
    Integer instanceNumber = this.getNextInstanceNumber();
    Product product = new Product();
    product.setCode("Product" + instanceNumber);
    product.setPrimaryName("Product" + instanceNumber);
    product.setDispensingUnit("unit" + instanceNumber);
    product.setDosesPerDispensingUnit(10);
    product.setPackSize(1);
    product.setPackRoundingThreshold(0);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(true);
    product.setTracer(false);
    product.setProductCategory(generateProductCategory());
    productRepository.save(product);
    return product;
  }

  private ProductCategory generateProductCategory() {
    Integer instanceNumber = this.getNextInstanceNumber();
    ProductCategory productCategory = new ProductCategory();
    productCategory.setCode("ProductCategor" + instanceNumber);
    productCategory.setName("vaccine" + instanceNumber);
    productCategory.setDisplayOrder(1);
    productCategoryRepository.save(productCategory);
    return productCategory;
  }
}
