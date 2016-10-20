package org.openlmis.requisition.domain;

import org.hibernate.annotations.Type;
import org.openlmis.requisition.dto.FacilityTypeApprovedProductDto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.UUID;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "requisition_line_items")
@NoArgsConstructor
public class RequisitionLineItem extends BaseEntity {

  private static final String UUID = "pg-uuid";

  @Getter
  @Setter
  @Type(type = UUID)
  private UUID orderableProductId;

  @ManyToOne(cascade = CascadeType.REFRESH)
  @JoinColumn(name = "requisitionId")
  @Getter
  @Setter
  private Requisition requisition;

  @Column
  @Getter
  @Setter
  private Integer beginningBalance;

  @Column
  @Getter
  @Setter
  private Integer totalReceivedQuantity;

  @Column
  @Getter
  @Setter
  private Integer totalLossesAndAdjustments;

  @Column
  @Getter
  @Setter
  private Integer stockOnHand;

  @Column
  @Getter
  @Setter
  private Integer requestedQuantity;

  @Column
  @Getter
  @Setter
  private Integer totalConsumedQuantity;

  @Column
  @Getter
  @Setter
  private String requestedQuantityExplanation;

  @Column(length = 250)
  @Getter
  @Setter
  private String remarks;

  @Column
  @Getter
  @Setter
  private Integer approvedQuantity;

  public RequisitionLineItem(Requisition requisition,
                             FacilityTypeApprovedProductDto facilityTypeApprovedProduct) {
    this.requisition = requisition;
    this.orderableProductId = facilityTypeApprovedProduct.getProgramProduct().getProductId();
  }

  /**
   * Copy values of attributes into new or updated RequisitionLineItem.
   *
   * @param requisitionLineItem RequisitionLineItem with new values.
   */
  public void updateFrom(RequisitionLineItem requisitionLineItem) {
    if (requisition.getStatus() == RequisitionStatus.AUTHORIZED) {
      this.approvedQuantity = requisitionLineItem.getApprovedQuantity();
      this.remarks = requisitionLineItem.getRemarks();
    } else {
      this.orderableProductId = requisitionLineItem.getOrderableProductId();
      this.stockOnHand = requisitionLineItem.getStockOnHand();
      this.beginningBalance = requisitionLineItem.getBeginningBalance();
      this.totalReceivedQuantity = requisitionLineItem.getTotalReceivedQuantity();
    }
  }

  /**
   * Calculate StockOnHand field value.
   */
  void calculateStockOnHand() {
    stockOnHand = zeroIfNull(beginningBalance) + zeroIfNull(totalReceivedQuantity)
        + zeroIfNull(totalLossesAndAdjustments) - zeroIfNull(totalConsumedQuantity);
  }

  /**
   * Calculate TotalConsumedQuantity field value.
   */
  void calculateTotalConsumedQuantity() {
    totalConsumedQuantity = zeroIfNull(beginningBalance) + zeroIfNull(totalReceivedQuantity)
            + zeroIfNull(totalLossesAndAdjustments) - zeroIfNull(stockOnHand);
  }

  boolean allRequiredCalcFieldsNotFilled(String field) {
    switch (field) {
      case "totalConsumedQuantity":
        return null == beginningBalance
            || null == totalReceivedQuantity
            || null == totalLossesAndAdjustments
            || null == stockOnHand;
      case "stockOnHand":
        return null == beginningBalance
            || null == totalReceivedQuantity
            || null == totalLossesAndAdjustments
            || null == totalConsumedQuantity;
      default:
        return false;
    }
  }

  private int zeroIfNull(Integer value) {
    return null == value ? 0 : value;
  }

  /**
   * Export this object to the specified exporter (DTO).
   *
   * @param exporter exporter to export to
   */
  public void export(Exporter exporter) {
    exporter.setId(id);
    exporter.setBeginningBalance(beginningBalance);
    exporter.setTotalReceivedQuantity(totalReceivedQuantity);
    exporter.setTotalLossesAndAdjustments(totalLossesAndAdjustments);
    exporter.setStockOnHand(stockOnHand);
    exporter.setRequestedQuantity(requestedQuantity);
    exporter.setTotalConsumedQuantity(totalConsumedQuantity);
    exporter.setRequestedQuantityExplanation(requestedQuantityExplanation);
    exporter.setRemarks(remarks);
    exporter.setApprovedQuantity(approvedQuantity);
  }

  public interface Exporter {
    void setId(UUID id);

    void setBeginningBalance(Integer beginningBalance);

    void setTotalReceivedQuantity(Integer totalReceivedQuantity);

    void setTotalLossesAndAdjustments(Integer totalLossesAndAdjustments);

    void setStockOnHand(Integer stockOnHand);

    void setRequestedQuantity(Integer requestedQuantity);

    void setTotalConsumedQuantity(Integer totalConsumedQuantity);

    void setRequestedQuantityExplanation(String requestedQuantityExplanation);

    void setRemarks(String remarks);

    void setApprovedQuantity(Integer approvedQuantity);
  }

  public interface Importer {
    UUID getId();

    Integer getBeginningBalance();

    Integer getTotalReceivedQuantity();

    Integer getTotalLossesAndAdjustments();

    Integer getStockOnHand();

    Integer getRequestedQuantity();

    Integer getTotalConsumedQuantity();

    String getRequestedQuantityExplanation();

    String getRemarks();

    Integer getApprovedQuantity();
  }
}
