package org.openlmis.referencedata.repository;

import com.google.common.collect.Lists;
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

  PeriodRepository getRepository() {
    return this.periodRepository;
  }

  private Schedule testSchedule;

  @Before
  public void setUp() {
    testSchedule = generateScheduleInstance("name", "code", "Test schedule");
    scheduleRepository.save(testSchedule);
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
  public void testFindByProcessingSchedule() {
    periodRepository.save(generatePeriodInstance("period" + getNextInstanceNumber(),
        testSchedule, "Test period", LocalDate.of(2016, 1, 1), LocalDate.of(2016, 2, 1)));
    Iterable<Period> result = periodRepository.findByProcessingSchedule(this.testSchedule);
    int size = Lists.newArrayList(result).size();
    Assert.assertEquals(1, size);
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

  @Test
  public void shouldFindPreviousPeriod() {
    Schedule secondSchedule = generateScheduleInstance("name2", "code2", "description");
    scheduleRepository.save(secondSchedule);

    Period firstPeriod = generatePeriodInstance(
        "first", secondSchedule, "", LocalDate.of(2016, 2, 1), LocalDate.of(2016, 3, 1));
    Period secondPeriod = generatePeriodInstance(
        "second", testSchedule, "", LocalDate.of(2016, 3, 1), LocalDate.of(2016, 4, 1));
    Period thirdPeriod = generatePeriodInstance(
        "third", testSchedule, "", LocalDate.of(2016, 4, 1), LocalDate.of(2016, 5, 1));
    Period fourthPeriod = generatePeriodInstance(
        "fourth", secondSchedule, "", LocalDate.of(2016, 5, 1), LocalDate.of(2016, 6, 1));

    periodRepository.save(firstPeriod);
    periodRepository.save(secondPeriod);
    periodRepository.save(thirdPeriod);
    periodRepository.save(fourthPeriod);

    Iterable<Period> previousPeriods = periodRepository.findPreviousPeriods(
        thirdPeriod.getProcessingSchedule(), thirdPeriod.getStartDate());

    Assert.assertEquals(previousPeriods.iterator().next(), secondPeriod);

    previousPeriods = periodRepository.findPreviousPeriods(
        fourthPeriod.getProcessingSchedule(), fourthPeriod.getStartDate());
    Assert.assertEquals(previousPeriods.iterator().next(), firstPeriod);

    previousPeriods = periodRepository.findPreviousPeriods(
        firstPeriod.getProcessingSchedule(), firstPeriod.getStartDate());
    Assert.assertFalse(previousPeriods.iterator().hasNext());
  }
}