package org.openlmis.referencedata.repository;

import org.junit.After;
import org.junit.Before;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.LocalDate;

public class RequisitionRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<Requisition> {

  private static final String requisitionRepository = "RequisitionRepositoryIntegrationTest";

  @Autowired
  private RequisitionRepository repository;

  @Autowired
  private ProgramRepository programRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private ScheduleRepository scheduleRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  private Program program = new Program();
  private Facility facility = new Facility();
  private Period period = new Period();
  private Schedule schedule = new Schedule();

  @Before
  public void setUp() {
    programRepository.deleteAll();
    program.setCode(requisitionRepository);
    programRepository.save(program);

    facilityTypeRepository.deleteAll();
    FacilityType facilityType = new FacilityType();
    facilityType.setCode(requisitionRepository);
    facilityTypeRepository.save(facilityType);

    geographicLevelRepository.deleteAll();
    GeographicLevel level = new GeographicLevel();
    level.setCode(requisitionRepository);
    level.setLevelNumber(1);
    geographicLevelRepository.save(level);

    geographicZoneRepository.deleteAll();
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(requisitionRepository);
    geographicZone.setLevel(level);
    geographicZoneRepository.save(geographicZone);

    facilityRepository.deleteAll();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode(requisitionRepository);
    facility.setActive(true);
    facility.setEnabled(true);
    facilityRepository.save(facility);

    scheduleRepository.deleteAll();
    schedule.setCode(requisitionRepository);
    schedule.setName(requisitionRepository);
    scheduleRepository.save(schedule);

    periodRepository.deleteAll();
    period.setName(requisitionRepository);
    period.setProcessingSchedule(schedule);
    period.setDescription(requisitionRepository);
    period.setStartDate(LocalDate.of(2016, 1, 1));
    period.setEndDate(LocalDate.of(2016, 2, 1));
    periodRepository.save(period);
  }

  RequisitionRepository getRepository() {
    return this.repository;
  }

  Requisition generateInstance() {
    repository.deleteAll();
    Requisition requisition = new Requisition();
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(period);
    requisition.setProgram(program);
    requisition.setStatus(RequisitionStatus.INITIATED);
    return requisition;
  }

  @After
  public void cleanUp() {
    periodRepository.deleteAll();
    scheduleRepository.deleteAll();
    facilityRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    programRepository.deleteAll();
    repository.deleteAll();
  }
}
