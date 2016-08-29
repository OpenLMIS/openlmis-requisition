package org.openlmis.referencedata.service;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.referencedata.domain.ProcessingPeriod;
import org.openlmis.referencedata.domain.ProcessingSchedule;
import org.openlmis.referencedata.repository.ProcessingPeriodRepository;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class ProcessingPeriodServiceTest {

  @Mock
  private ProcessingPeriodRepository periodRepository;

  @InjectMocks
  private ProcessingPeriodService periodService;

  private int currentInstanceNumber;
  private ProcessingSchedule testSchedule;

  private List<ProcessingPeriod> periods;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    periods = new ArrayList<>();
    testSchedule = generateSchedule();
    generateInstances();
  }

  @Test
  public void shouldFindPeriodsWithinProvidedDateIfTheyExist() {
    List<ProcessingPeriod> matchedPeriods = new ArrayList<>();
    matchedPeriods.addAll(periods);
    matchedPeriods.remove(periods.get(0));
    when(periodRepository
            .searchPeriods(testSchedule, periods.get(0).getStartDate()))
            .thenReturn(matchedPeriods);
    List<ProcessingPeriod> receivedPeriods = periodService
            .searchPeriods(testSchedule, periods.get(0).getStartDate());
    assertEquals(4,receivedPeriods.size());
    for ( ProcessingPeriod period : receivedPeriods) {
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

  private ProcessingPeriod generatePeriod() {
    Integer instanceNumber = generateInstanceNumber();
    ProcessingPeriod period = new ProcessingPeriod();
    period.setId(UUID.randomUUID());
    period.setName("PeriodName" + instanceNumber);
    period.setDescription("PeriodDescription" + instanceNumber);
    period.setEndDate(LocalDate.now().plusDays(instanceNumber));
    period.setStartDate(LocalDate.now().minusDays(instanceNumber));
    period.setProcessingSchedule(testSchedule);
    return period;
  }

  private ProcessingSchedule generateSchedule() {
    Integer instanceNumber = generateInstanceNumber();
    ProcessingSchedule schedule = new ProcessingSchedule();
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
