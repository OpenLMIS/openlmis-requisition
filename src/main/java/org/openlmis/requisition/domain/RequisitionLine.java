package org.openlmis.requisition.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.referencedata.domain.BaseEntity;
import org.openlmis.requisition.dto.ProductDto;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "requisition_lines")
@NoArgsConstructor
public class RequisitionLine extends BaseEntity {

  @ManyToOne
  @JoinColumn(name = "productId", nullable = false)
  @Getter
  @Setter
  private ProductDto product;

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

  /**
   * Copy values of attributes into new or updated RequisitionLine.
   *
   * @param  requisitionLine RequisitionLine with new values.
   */
  public void updateFrom(RequisitionLine requisitionLine) {
    this.product = requisitionLine.getProduct();
    this.requisition = requisitionLine.getRequisition();
    this.stockInHand = requisitionLine.getStockInHand();
    this.stockOnHand = requisitionLine.getStockOnHand();
    this.beginningBalance = requisitionLine.getBeginningBalance();
    this.totalReceivedQuantity = requisitionLine.getTotalReceivedQuantity();

  }
}
