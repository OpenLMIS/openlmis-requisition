package org.openlmis.requisition.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "stock_adjustments")
@NoArgsConstructor
public class StockAdjustment extends BaseEntity {

  @OneToOne
  @JoinColumn(name = "requisitionLineItemId")
  @Getter
  @Setter
  private RequisitionLineItem requisitionLineItem;

  @Column(nullable = false)
  @Getter
  @Setter
  private UUID reasonId;

  @Column(nullable = false)
  @Getter
  @Setter
  private Integer quantity;

  /**
   * Creates new StockAdjustment object based on data from {@link Importer}
   *
   * @param importer instance of {@link Importer}
   * @param requisitionLineItem RequisitionLineItem object
   * @return new instance of StockAdjustment.
   */
  public static StockAdjustment newStockAdjustment(Importer importer,
                                                   RequisitionLineItem requisitionLineItem) {
    StockAdjustment stockAdjustment = new StockAdjustment();
    stockAdjustment.id = importer.getId();
    stockAdjustment.requisitionLineItem = requisitionLineItem;
    stockAdjustment.reasonId = importer.getReasonId();
    stockAdjustment.quantity = importer.getQuantity();

    return stockAdjustment;
  }

  /**
   * Export this object to the specified exporter (DTO).
   *
   * @param exporter exporter to export to
   */
  public void export(StockAdjustment.Exporter exporter) {
    exporter.setId(id);
    exporter.setReasonId(reasonId);
    exporter.setQuantity(quantity);
  }

  public interface Exporter {
    void setId(UUID id);

    void setQuantity(Integer quantity);

    void setReasonId(UUID reasonId);
  }

  public interface Importer {
    UUID getId();

    UUID getReasonId();

    Integer getQuantity();
  }
}
