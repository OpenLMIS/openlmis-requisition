package org.openlmis.referencedata.repository;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.LocalDate;
import java.util.UUID;

public class PeriodRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<Period> {

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private ScheduleRepository scheduleRepository;

  private Schedule testSchedule;

  PeriodRepository getRepository() {
    return this.periodRepository;
  }

  @Before
  public void setUp() {
    scheduleRepository.deleteAll();
    periodRepository.deleteAll();
    testSchedule = generateScheduleInstance("name", "code", "Test schedule");
    scheduleRepository.save(testSchedule);
  }

  @After
  public void cleanUp() {
    scheduleRepository.deleteAll();
    periodRepository.deleteAll();
  }

  Period generateInstance() {
    int instanceNumber = this.getNextInstanceNumber();
    Period period = new Period();
    period.setName("period" + instanceNumber);
    period.setDescription("Test period");
    period.setStartDate(LocalDate.of(2016, 1, 1));
    period.setEndDate(LocalDate.of(2016, 2, 1));
    period.setProcessingSchedule(testSchedule);
    return period;
  }

  @Test
  public void testPeriodEdit() {
    Period periodFromRepo = generatePeriodInstance("period" + getNextInstanceNumber(),
        testSchedule, "Test period", LocalDate.of(2016, 1, 1), LocalDate.of(2016, 2, 1));
    periodFromRepo = periodRepository.save(periodFromRepo);
    UUID id = periodFromRepo.getId();
    periodFromRepo = periodRepository.findOne(id);
    String description = "New test description";
    Assert.assertNotEquals(description, periodFromRepo.getDescription());
    periodFromRepo.setDescription(description);
    periodFromRepo.setStartDate(LocalDate.of(2016, 2, 2));
    periodFromRepo.setEndDate(LocalDate.of(2016, 3, 2));
    periodRepository.save(periodFromRepo);
    Assert.assertEquals(description, periodFromRepo.getDescription());
  }

  private Schedule generateScheduleInstance(String name, String code, String description) {
    Schedule schedule = new Schedule();
    schedule.setName(name);
    schedule.setDescription(description);
    schedule.setCode(code);
    return schedule;
  }

  private Period generatePeriodInstance(
      String name, Schedule schedule, String description, LocalDate startDate, LocalDate endDate) {
    Period period = new Period();
    period.setName(name);
    period.setProcessingSchedule(schedule);
    period.setDescription(description);
    period.setStartDate(startDate);
    period.setEndDate(endDate);
    return period;
  }
}