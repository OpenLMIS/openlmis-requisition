package org.openlmis.referencedata.service;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
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
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.RequisitionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
@Transactional
public class RequisitionServiceTest {
  private static final String requisitionRepositoryName = "RequisitionRepositoryIntegrationTest";

  @Autowired
  private RequisitionService requisitionService;

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

  private Requisition requisition;

  @Before
  public void setUp() {
    requisitionRepository.deleteAll();
    periodRepository.deleteAll();
    scheduleRepository.deleteAll();
    facilityRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    programRepository.deleteAll();

    createTestRequisition();
  }

  @Test
  public void testTryDelete() {
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition);

    boolean deleted = requisitionService.tryDelete(requisition);
    Assert.assertTrue(deleted);
  }

  @Test
  public void testTryDeleteBadStatus() {
    requisition.setStatus(RequisitionStatus.SUBMITTED);
    requisitionRepository.save(requisition);

    boolean deleted = requisitionService.tryDelete(requisition);
    Assert.assertFalse(deleted);
  }

  @Test
  public void shouldSkipRequisition() {

    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.INITIATED);

    boolean skipResult = requisitionService.skip(requisition.getId());

    Assert.assertTrue(skipResult);
    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.SKIPPED);
  }

  @Test
  public void shouldNotSkipRequisitionIfProgramIsNotSkippable() {

    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.INITIATED);

    requisition.getProgram().setPeriodsSkippable(false);
    boolean skipResult = requisitionService.skip(requisition.getId());

    Assert.assertFalse(skipResult);
    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.INITIATED);

  }

  @Test
  public void shouldRejectRequisition() {

    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    requisitionRepository.save(requisition);

    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.AUTHORIZED);

    requisitionService.reject(requisition.getId());

    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.INITIATED);
  }

  @Test(expected = RequisitionException.class)
  public void shouldNotAllowRejectionIfRequisitionStatusIsWrong() {

    requisition.setStatus(RequisitionStatus.APPROVED);
    requisitionRepository.save(requisition);

    Assert.assertEquals(requisition.getStatus(), RequisitionStatus.APPROVED);

    requisitionService.reject(requisition.getId());
  }

  private void createTestRequisition() {
    Program program = new Program();
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
    scheduleRepository.save(schedule);

    Period period = new Period();
    period.setProcessingSchedule(schedule);
    periodRepository.save(period);

    requisition = new Requisition();
    requisition.setFacility(facility);
    requisition.setProcessingPeriod(period);
    requisition.setProgram(program);
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisitionRepository.save(requisition);
  }
}
