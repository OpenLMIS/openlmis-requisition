package org.openlmis.requisition.dto;

import java.util.UUID;
import lombok.Getter;
import lombok.Setter;

public class SupplyLineDto {

  @Getter
  @Setter
  private UUID id;

  @Getter
  @Setter
  private UUID supervisoryNode;

  @Getter
  @Setter
  private String description;

  @Getter
  @Setter
  private UUID program;

  @Getter
  @Setter
  private UUID supplyingFacility;

}
