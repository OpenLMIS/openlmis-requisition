package org.openlmis.referencedata.service;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.Application;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.repository.ScheduleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
public class PeriodServiceTest {

  @Autowired
  private PeriodService periodService;

  @Autowired
  private PeriodRepository periodRepository;

  @Autowired
  private ScheduleRepository scheduleRepository;

  private Integer currentInstanceNumber;
  private Schedule testSchedule;

  private List<Period> periods;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    periods = new ArrayList<>();
    testSchedule = generateSchedule();
    for ( int periodCount = 0; periodCount < 5; periodCount++ ) {
      periods.add(generatePeriod());
    }
  }

  @After
  public void cleanup() {
    periodRepository.deleteAll();
    scheduleRepository.deleteAll();
  }

  @Test
  public void testSearchPeriod() {
    List<Period> receivedPeriods =
            periodService.searchPeriods(
                    testSchedule,
                    periods.get(0).getStartDate());
    Assert.assertEquals(4,receivedPeriods.size());
    for ( Period period : receivedPeriods) {
      Assert.assertEquals(
              testSchedule.getId(),
              period.getProcessingSchedule().getId());
      Assert.assertTrue(
              periods.get(0).getStartDate().isAfter(period.getStartDate()));
    }
  }

  private Period generatePeriod() {
    Period period = new Period();
    Integer instanceNumber = generateInstanceNumber();
    period.setName("PeriodName" + instanceNumber);
    period.setDescription("PeriodDescription" + instanceNumber);
    period.setEndDate(LocalDate.now().plusDays(instanceNumber));
    period.setStartDate(LocalDate.now().minusDays(instanceNumber));
    period.setProcessingSchedule(testSchedule);
    periodRepository.save(period);
    return period;
  }

  private Schedule generateSchedule() {
    Schedule schedule = new Schedule();
    Integer instanceNumber = generateInstanceNumber();
    schedule.setCode("ScheduleCode" + instanceNumber);
    schedule.setDescription("ScheduleDescription" + instanceNumber);
    schedule.setName("ScheduleName" + instanceNumber);
    schedule.setModifiedDate(LocalDateTime.now());
    scheduleRepository.save(schedule);
    return schedule;
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }
}
