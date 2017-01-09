package org.openlmis.requisition.dto;

import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

@Getter
@Setter
public class SupplyLineDto {
  private UUID id;
  private UUID supervisoryNode;
  private String description;
  private UUID program;
  private UUID supplyingFacility;
}
