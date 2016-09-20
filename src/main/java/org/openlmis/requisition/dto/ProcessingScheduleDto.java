package org.openlmis.requisition.dto;

import java.util.UUID;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;

@NoArgsConstructor
public class ProcessingScheduleDto {

  @Getter
  @Setter
  private UUID id;

  @Getter
  @Setter
  private String code;

  @Getter
  @Setter
  private String description;

  @Getter
  @Setter
  private LocalDateTime modifiedDate;

  @Getter
  @Setter
  private String name;
}
