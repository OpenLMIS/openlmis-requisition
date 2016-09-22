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
@Table(name = "proof_of_delivery_line_items")
public class ProofOfDeliveryLineItem extends BaseEntity {

  @OneToOne
  @JoinColumn(name = "orderLineItemId", nullable = false)
  @Getter
  @Setter
  private OrderLineItem orderLineItem;

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
   * Copy values of attributes into new or updated ProofOfDeliveryLineItem.
   *
   * @param proofOfDeliveryLineItem ProofOfDeliveryLineItem with new values.
   */
  public void updateFrom(ProofOfDeliveryLineItem proofOfDeliveryLineItem) {
    this.orderLineItem = proofOfDeliveryLineItem.getOrderLineItem();
    this.proofOfDelivery = proofOfDeliveryLineItem.getProofOfDelivery();
    this.packToShip = proofOfDeliveryLineItem.getPackToShip();
    this.quantityShipped = proofOfDeliveryLineItem.getQuantityShipped();
    this.quantityReceived = proofOfDeliveryLineItem.getQuantityReceived();
    this.quantityReturned = proofOfDeliveryLineItem.getQuantityReturned();
    this.replacedProductCode = proofOfDeliveryLineItem.getReplacedProductCode();
    this.notes = proofOfDeliveryLineItem.getNotes();
  }
}
