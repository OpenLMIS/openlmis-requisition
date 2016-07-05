package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

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
  @JoinColumn(name = "requisitionId", nullable = false)
  @Getter
  @Setter
  private Requisition requisition;

  @ManyToOne
  @JoinColumn(name = "productId", nullable = false)
  @Getter
  @Setter
  private Product product;

  @Column(nullable = false)
  @Getter
  @Setter
  private Integer quantityRequested;
}
