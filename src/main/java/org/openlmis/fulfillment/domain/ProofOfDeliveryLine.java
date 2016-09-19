package org.openlmis.fulfillment.domain;

import lombok.Getter;
import lombok.Setter;
import org.openlmis.requisition.domain.BaseEntity;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "proof_of_delivery_lines")
public class ProofOfDeliveryLine extends BaseEntity {

  @OneToOne
  @JoinColumn(name = "orderLineId", nullable = false)
  @Getter
  @Setter
  private OrderLine orderLine;

  @ManyToOne(cascade = CascadeType.REFRESH)
  @JoinColumn(name = "proofOfDeliveryId", nullable = false)
  @Getter
  @Setter
  private ProofOfDelivery proofOfDelivery;

  @Column
  @Getter
  @Setter
  private Long packToShip;

  @Column
  @Getter
  @Setter
  private Long quantityShipped;

  @Column
  @Getter
  @Setter
  private Long quantityReceived;

  @Column
  @Getter
  @Setter
  private Long quantityReturned;

  @Column(columnDefinition = "text")
  @Getter
  @Setter
  private String replacedProductCode;

  @Column(columnDefinition = "text")
  @Getter
  @Setter
  private String notes;

  /**
   * Copy values of attributes into new or updated ProofOfDeliveryLine.
   *
   * @param proofOfDeliveryLine ProofOfDeliveryLine with new values.
   */
  public void updateFrom(ProofOfDeliveryLine proofOfDeliveryLine) {
    this.orderLine = proofOfDeliveryLine.getOrderLine();
    this.proofOfDelivery = proofOfDeliveryLine.getProofOfDelivery();
    this.packToShip = proofOfDeliveryLine.getPackToShip();
    this.quantityShipped = proofOfDeliveryLine.getQuantityShipped();
    this.quantityReceived = proofOfDeliveryLine.getQuantityReceived();
    this.quantityReturned = proofOfDeliveryLine.getQuantityReturned();
    this.replacedProductCode = proofOfDeliveryLine.getReplacedProductCode();
    this.notes = proofOfDeliveryLine.getNotes();
  }
}
