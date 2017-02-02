package org.openlmis.requisition.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class ApprovedProductDto {
  private UUID id;
  private ProgramOrderableDto programOrderable;
  private Double maxStock;
  private Double minStock;
  private Double emergencyOrderPoint;
}
