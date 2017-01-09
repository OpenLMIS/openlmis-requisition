package org.openlmis.requisition.dto;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import java.time.LocalDate;

public class ProcessingPeriodDtoTest {

  private static LocalDate startDate;
  private static LocalDate endDate;

  @Test
  public void shouldCalculateCorrectlyWhenTwoMonths() {
    startDate = LocalDate.of(2016, 10, 11);
    endDate = LocalDate.of(2016, 11, 30);

    assertTrue(isDurationAsExpected(startDate, endDate, 2));
  }

  @Test
  public void shouldCalculateCorrectlyWhenLessThanOneMonth() {
    startDate = LocalDate.of(2016, 1, 1);
    endDate = LocalDate.of(2016, 1, 20);

    assertTrue(isDurationAsExpected(startDate, endDate, 1));
  }

  @Test
  public void shouldCalculateCorrectlyWhenOneMonthAndAHalf() {
    startDate = LocalDate.of(2016, 1, 1);
    endDate = LocalDate.of(2016, 2, 16);

    assertTrue(isDurationAsExpected(startDate, endDate, 2));
  }

  @Test
  public void shouldCalculateCorrectlyWhenLastDayOfMonth() {
    startDate = LocalDate.of(2016, 1, 1);
    endDate = LocalDate.of(2016, 3, 31);

    assertTrue(isDurationAsExpected(startDate, endDate, 3));
  }

  @Test
  public void shouldCalculateCorrectlyWhenMonthAndOneDay() {
    startDate = LocalDate.of(2016, 1, 1);
    endDate = LocalDate.of(2016, 2, 2);

    assertTrue(isDurationAsExpected(startDate, endDate, 1));
  }

  @Test
  public void shouldCalculateCorrectlyWhenLastDayOfFirstMonth() {
    startDate = LocalDate.of(2016, 1, 1);
    endDate = LocalDate.of(2016, 1, 31);

    assertTrue(isDurationAsExpected(startDate, endDate, 1));
  }

  @Test
  public void shouldCalculateCorrectlyWhenLessThanTwoWeeks() {
    startDate = LocalDate.of(2016, 1, 1);
    endDate = LocalDate.of(2016, 1, 10);

    assertTrue(isDurationAsExpected(startDate, endDate, 1));
  }

  @Test
  public void shouldCalculateCorrectlyWhenMoreThanOneYear() {
    startDate = LocalDate.of(2016, 1, 1);
    endDate = LocalDate.of(2017, 2, 2);

    assertTrue(isDurationAsExpected(startDate, endDate, 13));
  }


  private boolean isDurationAsExpected(LocalDate start, LocalDate end, int expected) {
    ProcessingPeriodDto period = new ProcessingPeriodDto();
    period.setStartDate(start);
    period.setEndDate(end);
    return period.getDurationInMonths() == expected;
  }

}