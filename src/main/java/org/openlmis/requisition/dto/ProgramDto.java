package org.openlmis.requisition.dto;

import java.util.UUID;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ProgramDto {

  @Getter
  @Setter
  private UUID id;

  @Getter
  @Setter
  private String code;

  @Getter
  @Setter
  private String name;

  @Getter
  @Setter
  private String description;

  @Getter
  @Setter
  private Boolean active;

  @Getter
  @Setter
  private Boolean periodsSkippable;

  @Getter
  @Setter
  private Boolean showNonFullSupplyTab;
}
