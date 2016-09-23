package org.openlmis.requisition.dto;

import java.util.UUID;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;

@Getter
@Setter
public class ProcessingScheduleDto {
  private UUID id;
  private String code;
  private String description;
  private LocalDateTime modifiedDate;
  private String name;
}
