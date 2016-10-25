package org.openlmis.requisition.dto;

import org.openlmis.requisition.domain.StockAdjustment;

import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

@Getter
@Setter
public class StockAdjustmentDto implements StockAdjustment.Importer, StockAdjustment.Exporter {
  private UUID id;
  private UUID reasonId;
  private Integer quantity;
}
