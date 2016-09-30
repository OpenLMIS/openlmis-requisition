package org.openlmis.requisition.domain;

import org.openlmis.requisition.dto.FacilityTypeApprovedProductDto;

import java.util.UUID;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "requisition_line_items")
@NoArgsConstructor
public class RequisitionLineItem extends BaseEntity {

  @Getter
  @Setter
  private UUID orderableProduct;

  @ManyToOne(cascade = CascadeType.REFRESH)
  @JoinColumn(name = "requisitionId")
  @Getter
  @Setter
  private Requisition requisition;

  @Column
  @Getter
  @Setter
  private Integer stockInHand;

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

  public RequisitionLineItem(FacilityTypeApprovedProductDto facilityTypeApprovedProduct) {
    // currently nothing to do here
    // future tickets related with requisition should add some steps
  }

  /**
   * Copy values of attributes into new or updated RequisitionLineItem.
   *
   * @param  requisitionLineItem RequisitionLineItem with new values.
   */
  public void updateFrom(RequisitionLineItem requisitionLineItem) {
    this.orderableProduct = requisitionLineItem.getOrderableProduct();
    this.requisition = requisitionLineItem.getRequisition();
    this.stockInHand = requisitionLineItem.getStockInHand();
    this.stockOnHand = requisitionLineItem.getStockOnHand();
    this.beginningBalance = requisitionLineItem.getBeginningBalance();
    this.totalReceivedQuantity = requisitionLineItem.getTotalReceivedQuantity();
  }

  /**
   * Calculate StockOnHand field value.
   */
  void calculateStockOnHand() {
    stockOnHand = beginningBalance + totalReceivedQuantity
        + totalLossesAndAdjustments - totalConsumedQuantity;
  }
}
