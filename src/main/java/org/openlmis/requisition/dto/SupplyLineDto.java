package org.openlmis.requisition.dto;

import java.util.UUID;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class SupplyLineDto {
  private UUID id;
  private UUID supervisoryNode;
  private String description;
  private UUID program;
  private UUID supplyingFacility;
}
