package org.openlmis.requisition.dto;

import com.fasterxml.jackson.annotation.JsonFormat;

import lombok.Getter;
import lombok.Setter;

import java.time.LocalDate;
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
  private boolean active;

  @JsonFormat(pattern = "yyyy-MM-dd")
  private LocalDate startDate;
}
