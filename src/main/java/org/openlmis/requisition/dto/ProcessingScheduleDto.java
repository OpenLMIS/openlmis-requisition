package org.openlmis.requisition.dto;

import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.UUID;

@Getter
@Setter
public class ProcessingScheduleDto {
  private UUID id;
  private String code;
  private String description;
  private LocalDateTime modifiedDate;
  private String name;
}
