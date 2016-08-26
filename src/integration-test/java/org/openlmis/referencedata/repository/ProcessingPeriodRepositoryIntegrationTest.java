package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.referencedata.domain.ProcessingSchedule;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class ProcessingPeriodRepositoryIntegrationTest
      extends BaseCrudRepositoryIntegrationTest<ProcessingPeriod> {

  @Autowired
  private ProcessingPeriodRepository periodRepository;

  @Autowired
  private ProcessingScheduleRepository scheduleRepository;

  private static final String PERIOD_NAME = "name";
  private static final String PERIOD_DESCRIPTION = "description";

  private ProcessingSchedule testSchedule;

  ProcessingPeriodRepository getRepository() {
    return this.periodRepository;
  }

  @Before
  public void setUp() {
    testSchedule = generateScheduleInstance(PERIOD_NAME, "code", "Test schedule");
    scheduleRepository.save(testSchedule);
  }

  private ProcessingSchedule generateScheduleInstance(String name, String code,
                                                      String description) {
    ProcessingSchedule schedule = new ProcessingSchedule();
    schedule.setName(name);
    schedule.setDescription(description);
    schedule.setCode(code);
    return schedule;
  }

  private ProcessingPeriod generatePeriodInstance(
        String name, ProcessingSchedule schedule, String description,
        LocalDate startDate, LocalDate endDate) {
    ProcessingPeriod period = new ProcessingPeriod();
    period.setName(name);
    period.setProcessingSchedule(schedule);
    period.setDescription(description);
    period.setStartDate(startDate);
    period.setEndDate(endDate);
    return period;
  }

  ProcessingPeriod generateInstance() {
    int instanceNumber = this.getNextInstanceNumber();
    ProcessingPeriod period = new ProcessingPeriod();
    period.setName("period" + instanceNumber);
    period.setDescription("Test period");
    period.setStartDate(LocalDate.of(2016, 1, 1));
    period.setEndDate(LocalDate.of(2016, 2, 1));
    period.setProcessingSchedule(testSchedule);
    return period;
  }

  @Test
  public void testPeriodEdit() {
    ProcessingPeriod periodFromRepo = generatePeriodInstance("period" + getNextInstanceNumber(),
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
  public void testSearchPeriodsByAllParameters() {
    List<ProcessingPeriod> periods = new ArrayList<>();
    for (int periodsCount = 0; periodsCount < 5; periodsCount++) {
      periods.add(generatePeriodInstance(
              PERIOD_NAME + periodsCount,
              testSchedule,
              PERIOD_DESCRIPTION + periodsCount,
              LocalDate.now().minusDays(periodsCount),
              LocalDate.now().plusDays(periodsCount)));
      periodRepository.save(periods.get(periodsCount));
    }
    List<ProcessingPeriod> receivedPeriods =
            periodRepository.searchPeriods(testSchedule, periods.get(1).getStartDate());

    Assert.assertEquals(4, receivedPeriods.size());
    for (ProcessingPeriod period : receivedPeriods) {
      Assert.assertEquals(
              testSchedule.getId(),
              period.getProcessingSchedule().getId());
      Assert.assertTrue(
              period.getStartDate().isBefore(periods.get(0).getStartDate()));
    }
  }

  @Test
  public void testSearchPeriodsByDateTo() {
    List<ProcessingPeriod> periods = new ArrayList<>();
    for (int periodsCount = 0; periodsCount < 5; periodsCount++) {
      periods.add(generatePeriodInstance(
              PERIOD_NAME + periodsCount,
              testSchedule,
              PERIOD_DESCRIPTION + periodsCount,
              LocalDate.now().minusDays(periodsCount),
              LocalDate.now().plusDays(periodsCount)));
      periodRepository.save(periods.get(periodsCount));
    }
    List<ProcessingPeriod> receivedPeriods =
            periodRepository.searchPeriods(null, periods.get(1).getStartDate());

    Assert.assertEquals(4, receivedPeriods.size());
    for (ProcessingPeriod period : receivedPeriods) {
      Assert.assertTrue(
              period.getStartDate().isBefore(periods.get(0).getStartDate()));
    }
  }

  @Test
  public void testSearchPeriodsByAllParametersNull() {
    List<ProcessingPeriod> periods = new ArrayList<>();
    for (int periodsCount = 0; periodsCount < 5; periodsCount++) {
      periods.add(generatePeriodInstance(
              PERIOD_NAME + periodsCount,
              testSchedule,
              PERIOD_DESCRIPTION + periodsCount,
              LocalDate.now().minusDays(periodsCount),
              LocalDate.now().plusDays(periodsCount)));
      periodRepository.save(periods.get(periodsCount));
    }
    List<ProcessingPeriod> receivedPeriods =
            periodRepository.searchPeriods(null, null);

    Assert.assertEquals(periods.size(), receivedPeriods.size());
  }
}