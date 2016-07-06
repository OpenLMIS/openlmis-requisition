package org.openlmis.referencedata.repository;

import org.junit.Before;
import org.openlmis.product.repository.ProductRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.requisition.repository.RequisitionLineRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.LocalDate;

public class RequisitionLineRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<RequisitionLine> {

  private static final String requisitionLineRepository =
      "RequisitionLineRepositoryIntegrationTest";

  @Autowired
  RequisitionLineRepository repository;

  @Autowired
  RequisitionRepository requisitionRepository;

  @Autowired
  ProductRepository productRepository;

  @Autowired
  ProgramRepository programRepository;

  @Autowired
  FacilityRepository facilityRepository;

  @Autowired
  ScheduleRepository scheduleRepository;

  @Autowired
  PeriodRepository periodRepository;

  private Requisition requisition = new Requisition();
  private Product product = new Product();

  /**
   * Prepare the test environment.
   */
  @Before
  public void setUp() {
    productRepository.deleteAll();
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
    productRepository.save(product);

    Program program = new Program();
    programRepository.deleteAll();
    program.setCode(requisitionLineRepository);
    programRepository.save(program);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode(requisitionLineRepository);
    GeographicLevel level = new GeographicLevel();
    level.setCode(requisitionLineRepository);
    level.setLevelNumber(1);
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(requisitionLineRepository);
    geographicZone.setLevel(level);

    Facility facility = new Facility();
    facilityRepository.deleteAll();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(requisitionLineRepository);
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    Schedule schedule = new Schedule();
    scheduleRepository.deleteAll();
    schedule.setCode(requisitionLineRepository);
    schedule.setName(requisitionLineRepository);
    scheduleRepository.save(schedule);

    Period period = new Period();
    periodRepository.deleteAll();
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
    requisitionRepository.save(requisition);
  }

  RequisitionLineRepository getRepository() {
    return this.repository;
  }

  RequisitionLine generateInstance() {
    repository.deleteAll();
    RequisitionLine requisitionLine = new RequisitionLine();
    requisitionLine.setRequisition(requisition);
    requisitionLine.setProduct(product);
    requisitionLine.setQuantityRequested(1);
    return requisitionLine;
  }
}
