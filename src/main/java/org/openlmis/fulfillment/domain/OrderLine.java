package org.openlmis.fulfillment.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.requisition.domain.BaseEntity;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import java.util.UUID;

@Entity
@Table(name = "order_lines")
@NoArgsConstructor
public class OrderLine extends BaseEntity {

  @ManyToOne(cascade = CascadeType.REFRESH)
  @JoinColumn(name = "orderId", nullable = false)
  @Getter
  @Setter
  private Order order;

  @Getter
  @Setter
  private UUID product;

  @Column(nullable = false)
  @Getter
  @Setter
  private Long orderedQuantity;

  @Column(nullable = false)
  @Getter
  @Setter
  private Long filledQuantity;

  /**
   * Copy values of attributes into new or updated OrderLine.
   *
   * @param orderLine OrderLine with new values.
   */
  public void updateFrom(OrderLine orderLine) {
    this.order = orderLine.getOrder();
    this.product = orderLine.getProduct();
    this.orderedQuantity = orderLine.getOrderedQuantity();
    this.filledQuantity = orderLine.getFilledQuantity();
  }
}
