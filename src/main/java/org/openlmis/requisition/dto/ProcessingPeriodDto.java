package org.openlmis.requisition.dto;

import java.time.LocalDate;
import java.util.UUID;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ProcessingPeriodDto {
  private UUID id;
  private UUID processingSchedule;
  private String name;
  private String description;
  private LocalDate startDate;
  private LocalDate endDate;
}
