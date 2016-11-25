package org.openlmis.requisition.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "stock_adjustments")
@NoArgsConstructor
public class StockAdjustment extends BaseEntity {

  @Column(nullable = false)
  @Getter
  @Setter
  private UUID reasonId;

  @Column(nullable = false)
  @Getter
  @Setter
  private Integer quantity;

  public StockAdjustment(UUID reasonId, Integer quantity) {
    this.reasonId = reasonId;
    this.quantity = quantity;
  }

  /**
   * Creates new StockAdjustment object based on data from {@link Importer}
   *
   * @param importer instance of {@link Importer}
   * @return new instance of StockAdjustment.
   */
  public static StockAdjustment newStockAdjustment(Importer importer) {
    StockAdjustment stockAdjustment = new StockAdjustment(
        importer.getReasonId(), importer.getQuantity());
    stockAdjustment.id = importer.getId();

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
