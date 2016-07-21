package org.openlmis.referencedata.repository;

import org.junit.Before;
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

public class RequisitionLineRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<RequisitionLine> {

  private static final String requisitionLineRepository =
      "RequisitionLineRepositoryIntegrationTest";

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
  ProductCategoryRepository productCategoryRepository;

  private Requisition requisition = new Requisition();
  private Product product = new Product();

  /** Prepare the test environment. */
  @Before
  public void setUp() {
    productRepository.deleteAll();
    productCategoryRepository.deleteAll();

    ProductCategory productCategory1 = new ProductCategory();
    productCategory1.setCode("PC1");
    productCategory1.setName("PC1 name");
    productCategory1.setDisplayOrder(1);
    productCategoryRepository.save(productCategory1);

    product.setCode(requisitionLineRepository);
    product.setPrimaryName(requisitionLineRepository);
    product.setDispensingUnit(requisitionLineRepository);
    product.setDosesPerDispensingUnit(10);
    product.setPackSize(1);
    product.setPackRoundingThreshold(0);
    product.setRoundToZero(false);
    product.setActive(true);
    product.setFullSupply(true);
    product.setTracer(false);
    product.setProductCategory(productCategory1);
    productRepository.save(product);

    programRepository.deleteAll();
    Program program = new Program();
    program.setCode(requisitionLineRepository);
    programRepository.save(program);

    facilityTypeRepository.deleteAll();
    FacilityType facilityType = new FacilityType();
    facilityType.setCode(requisitionLineRepository);
    facilityTypeRepository.save(facilityType);

    geographicLevelRepository.deleteAll();
    GeographicLevel level = new GeographicLevel();
    level.setCode(requisitionLineRepository);
    level.setLevelNumber(1);
    geographicLevelRepository.save(level);

    geographicZoneRepository.deleteAll();
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(requisitionLineRepository);
    geographicZone.setLevel(level);
    geographicZoneRepository.save(geographicZone);

    facilityRepository.deleteAll();
    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(requisitionLineRepository);
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    scheduleRepository.deleteAll();
    Schedule schedule = new Schedule();
    schedule.setCode(requisitionLineRepository);
    schedule.setName(requisitionLineRepository);
    scheduleRepository.save(schedule);

    periodRepository.deleteAll();
    Period period = new Period();
    period.setName(requisitionLineRepository);
    period.setProcessingSchedule(schedule);
    period.setDescription(requisitionLineRepository);
    period.setStartDate(LocalDate.of(2016, 1, 1));
    period.setEndDate(LocalDate.of(2016, 2, 1));
    periodRepository.save(period);

    requisitionRepository.deleteAll();
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(period);
    requisition.setProgram(program);
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition);
  }

  RequisitionLineRepository getRepository() {
    return this.repository;
  }

  RequisitionLine generateInstance() {
    repository.deleteAll();
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setProduct(product);
    requisitionLine.setQuantityRequested(1);
    return requisitionLine;
  }
}
