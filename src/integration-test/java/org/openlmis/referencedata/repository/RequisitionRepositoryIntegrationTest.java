package org.openlmis.referencedata.repository;

import org.junit.Before;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Requisition;
import org.openlmis.referencedata.domain.Schedule;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.LocalDate;

public class RequisitionRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<Requisition> {

  private static final String requisitionRepository = "RequisitionRepositoryIntegrationTest";

  @Autowired
  RequisitionRepository repository;

  @Autowired
  ProgramRepository programRepository;

  @Autowired
  FacilityRepository facilityRepository;

  @Autowired
  PeriodRepository periodRepository;

  @Autowired
  ScheduleRepository scheduleRepository;

  private Program program = new Program();
  private Facility facility = new Facility();
  private Period period = new Period();
  private Schedule schedule = new Schedule();

  /**
   * Prepare the test environment.
   */
  @Before
  public void setUp() {
    programRepository.deleteAll();
    program.setCode(requisitionRepository);
    programRepository.save(program);

    FacilityType facilityType = new FacilityType();
    facilityType.setCode(requisitionRepository);
    GeographicLevel level = new GeographicLevel();
    level.setCode(requisitionRepository);
    level.setLevelNumber(1);
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode(requisitionRepository);
    geographicZone.setLevel(level);

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
    return requisition;
  }
}
