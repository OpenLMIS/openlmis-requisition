package org.openlmis.referencedata.service;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.PeriodRepository;
import org.openlmis.referencedata.service.PeriodService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

@Transactional
public class PeriodServiceTest {

  @Mock
  private PeriodRepository periodRepository;

  @InjectMocks
  @Autowired
  private PeriodService periodService;

  private Period period;
  private Schedule schedule;

  private Integer currentInstanceNumber;
  private Schedule testSchedule;

  private List<Period> periods;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    periods = new ArrayList<>();
    testSchedule = generateSchedule();
    generateInstances();
    initMocks(this);
    mockRepositories();
  }

  @Test
  public void testSearchPeriod() {
    List<Period> receivedPeriods = periodService
            .searchPeriods(testSchedule, periods.get(0).getStartDate());
    Assert.assertEquals(4,receivedPeriods.size());
    for ( Period period : receivedPeriods) {
      Assert.assertEquals(
              testSchedule.getId(),
              period.getProcessingSchedule().getId());
      Assert.assertTrue(
              periods.get(0).getStartDate().isAfter(period.getStartDate()));
    }
  }

  private void generateInstances() {
    for ( int periodCount = 0; periodCount < 5; periodCount++ ) {
      periods.add(generatePeriod());
    }
  }

  private Period generatePeriod() {
    Integer instanceNumber = generateInstanceNumber();
    period = new Period();
    period.setId(UUID.randomUUID());
    period.setName("PeriodName" + instanceNumber);
    period.setDescription("PeriodDescription" + instanceNumber);
    period.setEndDate(LocalDate.now().plusDays(instanceNumber));
    period.setStartDate(LocalDate.now().minusDays(instanceNumber));
    period.setProcessingSchedule(testSchedule);
    return period;
  }

  private Schedule generateSchedule() {
    Integer instanceNumber = generateInstanceNumber();
    schedule = new Schedule();
    schedule.setId(UUID.randomUUID());
    schedule.setCode("ScheduleCode" + instanceNumber);
    schedule.setDescription("ScheduleDescription" + instanceNumber);
    schedule.setName("ScheduleName" + instanceNumber);
    schedule.setModifiedDate(LocalDateTime.now());
    return schedule;
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }

  private void mockRepositories() {
    for (Period period : periods) {
      List<Period> matchedPeriods = new ArrayList<>();
      for (Period periodWithMatchedProcessingScheduleAndToDate : periods) {
        if (periodWithMatchedProcessingScheduleAndToDate.getProcessingSchedule().equals(
                period.getProcessingSchedule()) && periodWithMatchedProcessingScheduleAndToDate
                .getStartDate().isBefore(period.getStartDate())) {
          matchedPeriods.add(periodWithMatchedProcessingScheduleAndToDate);
        }
      }
      when(periodRepository
              .searchPeriods(period.getProcessingSchedule(), period.getStartDate()))
              .thenReturn(matchedPeriods);
    }
    when(periodRepository
            .findOne(period.getId()))
            .thenReturn(period);
    when(periodRepository
            .save(period))
            .thenReturn(period);
  }
}
