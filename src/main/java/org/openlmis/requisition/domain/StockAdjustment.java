package org.openlmis.requisition.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.requisition.dto.StockAdjustmentReasonDto;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.util.UUID;

@Entity
@Table(name = "stock_adjustments")
@NoArgsConstructor
public class StockAdjustment extends BaseEntity {
  @OneToOne
  @JoinColumn(name = "requisitionLineItemId")
  @Getter
  @Setter
  private RequisitionLineItem requisitionLineItem;

  @Getter
  @Setter
  private UUID reasonId;

  @Getter
  @Setter
  private Integer quantity;

  /**
   * Export this object to the specified exporter (DTO).
   *
   * @param exporter exporter to export to
   */
  public void export(StockAdjustment.Exporter exporter) {
    exporter.setId(id);
    exporter.setRequisitionLineItem(requisitionLineItem);
    exporter.setQuantity(quantity);
  }

  public interface Exporter {
    void setId(UUID id);

    void setRequisitionLineItem(RequisitionLineItem requisitionLineItem);

    void setQuantity(Integer quantity);
  }

  public interface Importer {
    UUID getId();

    RequisitionLineItem.Importer getRequisitionLineItem();

    StockAdjustmentReasonDto getReason();

    Integer getQuantity();
  }
}
