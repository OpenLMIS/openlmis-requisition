package org.openlmis.requisition.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.UUID;

@Getter
@Setter
public class SupportedProgramDto {
  private UUID id;
  private String code;
  private String name;
  private String description;
  private boolean programActive;
  private boolean periodsSkippable;
  private boolean showNonFullSupplyTab;
  private boolean supportActive;
  private String supportStartDate;

  /**
   * Get supportStartDate from string and turn it into ZonedDateTime. Use midnight for time and UTC
   * for zone. If supportStartDate is null, return null.
   */
  @JsonIgnore
  public ZonedDateTime getZonedStartDate() {
    return (supportStartDate == null) ? null : ZonedDateTime.of(LocalDate.parse(supportStartDate),
        LocalTime.MIDNIGHT, ZoneId.of("UTC"));
  }

}
