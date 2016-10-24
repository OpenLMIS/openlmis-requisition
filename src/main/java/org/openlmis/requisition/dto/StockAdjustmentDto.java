package org.openlmis.requisition.dto;

import lombok.Getter;
import lombok.Setter;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.StockAdjustment;

import java.util.UUID;

@Getter
@Setter
public class StockAdjustmentDto implements StockAdjustment.Importer, StockAdjustment.Exporter {
  private UUID id;
  private RequisitionLineItemDto requisitionLineItem;
  private StockAdjustmentReasonDto reason;
  private Integer quantity;

  @Override
  public void setRequisitionLineItem(RequisitionLineItem requisitionLineItem) {
    requisitionLineItem.export(this.requisitionLineItem);
  }
}
