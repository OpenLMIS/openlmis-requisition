package org.openlmis.requisition.dto;

import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

@Getter
@Setter
public class StockAdjustmentReasonDto {
  private UUID id;
  private UUID programId;
  private String name;
  private String description;
  private Boolean additive;
  private Integer displayOrder;
}
