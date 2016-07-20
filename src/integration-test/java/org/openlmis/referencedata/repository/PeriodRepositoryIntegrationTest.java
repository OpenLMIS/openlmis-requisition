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

  private Schedule schedule;

  /** Prepare the test environment. */
  @Before
  public void setUp() {
    scheduleRepository.deleteAll();
    schedule = new Schedule();
    schedule.setCode("code");
    schedule.setName("schedule");
    schedule.setDescription("Test schedule");
    scheduleRepository.save(schedule);
  }

  Period generateInstance() {
    int instanceNumber = this.getNextInstanceNumber();
    Period period = new Period();
    period.setName("period" + instanceNumber);
    period.setProcessingSchedule(schedule);
    period.setDescription("Test period");
    period.setStartDate(LocalDate.of(2016, 1, 1));
    period.setEndDate(LocalDate.of(2016, 2, 1));
    return period;
  }

  @Test
  public void testFindByProcessingSchedule() {
    periodRepository.save(this.generateInstance());
    Iterable<Period> result = periodRepository.findByProcessingSchedule(this.schedule);
    int size = Lists.newArrayList(result).size();
    Assert.assertEquals(1, size);
  }

  @Test
  public void testPeriodEdit() {
    Period periodFromRepo = this.generateInstance();
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
}