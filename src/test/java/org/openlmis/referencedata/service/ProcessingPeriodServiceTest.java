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
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class ProcessingPeriodServiceTest {

  @Mock
  private ProcessingPeriodRepository periodRepository;

  @InjectMocks
  private ProcessingPeriodService periodService;

  private ProcessingSchedule schedule;
  private List<ProcessingPeriod> periods;

  @Before
  public void setUp() {
    periods = new ArrayList<>();
    schedule = mock(ProcessingSchedule.class);
    generateInstances();
  }

  @Test
  public void shouldFindPeriodsWithinProvidedDateIfTheyExist() {
    List<ProcessingPeriod> matchedPeriods = new ArrayList<>();
    matchedPeriods.addAll(periods);
    matchedPeriods.remove(periods.get(0));
    when(periodRepository
            .searchPeriods(schedule, periods.get(0).getStartDate()))
            .thenReturn(matchedPeriods);

    List<ProcessingPeriod> receivedPeriods = periodService
            .searchPeriods(schedule, periods.get(0).getStartDate());

    assertEquals(4, receivedPeriods.size());
    for (ProcessingPeriod period : receivedPeriods) {
      assertEquals(schedule, period.getProcessingSchedule());
      assertTrue(periods.get(0).getStartDate().isAfter(period.getStartDate()));
    }
  }

  private void generateInstances() {
    final int periodCount = 5;
    for (int i = 0; i < periodCount; i++) {
      periods.add(generatePeriod(i));
    }
  }

  private ProcessingPeriod generatePeriod(int instanceNumber) {
    ProcessingPeriod period = new ProcessingPeriod();
    period.setId(UUID.randomUUID());
    period.setName("PeriodName" + instanceNumber);
    period.setDescription("PeriodDescription" + instanceNumber);
    period.setEndDate(LocalDate.now().plusDays(instanceNumber));
    period.setStartDate(LocalDate.now().minusDays(instanceNumber));
    period.setProcessingSchedule(schedule);
    return period;
  }
}
