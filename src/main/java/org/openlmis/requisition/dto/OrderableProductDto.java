package org.openlmis.requisition.dto;

import lombok.Getter;
import lombok.Setter;

import java.util.Set;
import java.util.UUID;

@Getter
@Setter
public class OrderableProductDto {
  private UUID id;
  private String dispensable;
  private String productCode;
  private String name;
  private long packSize;
  private long packRoundingThreshold;
  private boolean roundToZero;
  private Set<ProgramProductDto> programs;
}
