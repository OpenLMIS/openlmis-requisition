package org.openlmis.referencedata.repository;

import org.junit.Before;
import org.openlmis.hierarchyandsupervision.repository.RequisitionGroupProgramScheduleRepository;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.hierarchyandsupervision.domain.RequisitionGroupProgramSchedule;
import org.openlmis.referencedata.domain.Schedule;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Allow testing requisitionGroupProgramScheduleRepository.
 */
public class RequisitionGroupProgramScheduleRepositoryIntegrationTest
    extends BaseCrudRepositoryIntegrationTest<RequisitionGroupProgramSchedule> {

  @Autowired
  RequisitionGroupProgramScheduleRepository repository;

  @Autowired
  ProgramRepository programRepository;

  @Autowired
  ScheduleRepository scheduleRepository;

  private Program program;
  private Schedule schedule;

  RequisitionGroupProgramScheduleRepository getRepository() {
    return this.repository;
  }

  RequisitionGroupProgramSchedule generateInstance() {
    RequisitionGroupProgramSchedule requisitionGroupProgramSchedule =
        new RequisitionGroupProgramSchedule();
    requisitionGroupProgramSchedule.setProgram(program);
    requisitionGroupProgramSchedule.setProcessingSchedule(schedule);
    requisitionGroupProgramSchedule.setDirectDelivery(false);
    return requisitionGroupProgramSchedule;
  }

  /** Allow setup environment before each test. */
  @Before
  public void setUp() {
    final String code = "RequisitionGroup";

    programRepository.deleteAll();
    scheduleRepository.deleteAll();
    program = new Program();
    program.setCode(code);
    programRepository.save(program);

    schedule = new Schedule();
    schedule.setCode(code);
    schedule.setName(code);
    scheduleRepository.save(schedule);
  }
}
