package org.openlmis.referencedata.service;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Schedule;
import org.openlmis.referencedata.repository.PeriodRepository;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

public class PeriodServiceTest {

  @Mock
  private PeriodRepository periodRepository;

  @InjectMocks
  private PeriodService periodService;

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
  }

  @Test
  public void shouldFindPeriodsWithinProvidedDateIfTheyExist() {
    List<Period> matchedPeriods = new ArrayList<>();
    matchedPeriods.addAll(periods);
    matchedPeriods.remove(periods.get(0));
    when(periodRepository
            .searchPeriods(testSchedule, periods.get(0).getStartDate()))
            .thenReturn(matchedPeriods);
    List<Period> receivedPeriods = periodService
            .searchPeriods(testSchedule, periods.get(0).getStartDate());
    assertEquals(4,receivedPeriods.size());
    for ( Period period : receivedPeriods) {
      assertEquals(
              testSchedule.getId(),
              period.getProcessingSchedule().getId());
      assertTrue(
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
    Period period = new Period();
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
    Schedule schedule = new Schedule();
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
}
