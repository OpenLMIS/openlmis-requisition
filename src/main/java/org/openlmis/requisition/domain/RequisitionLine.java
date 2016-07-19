package org.openlmis.requisition.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.BaseEntity;

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
  private Product product;

  @ManyToOne
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
  private Integer quantityRequested;

  @Column
  @Getter
  @Setter
  private Integer beginningBalance;
  
}
