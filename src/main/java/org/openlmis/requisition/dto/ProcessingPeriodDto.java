package org.openlmis.requisition.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.Getter;
import lombok.Setter;

import java.time.LocalDate;
import java.time.Period;
import java.util.UUID;

@Getter
@Setter
public class ProcessingPeriodDto {
  private UUID id;
  private ProcessingScheduleDto processingSchedule;
  private String name;
  private String description;
  private LocalDate startDate;
  private LocalDate endDate;

  /**
   * Returns duration of period in months.
   *
   * @return number od months.
   */
  @JsonIgnore
  public int getDurationInMonths() {
    Period length = Period.between(startDate, endDate);
    int months = length.getMonths();
    months += length.getYears() * 12;
    if (length.getDays() >= 15 || months == 0) {
      months++;
    }

    return months;
  }
}
