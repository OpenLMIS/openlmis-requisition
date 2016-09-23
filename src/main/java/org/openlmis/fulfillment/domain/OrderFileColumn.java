package org.openlmis.fulfillment.domain;

import lombok.AllArgsConstructor;
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

@Entity
@Table(name = "order_file_columns")
@NoArgsConstructor
@AllArgsConstructor
public class OrderFileColumn extends BaseEntity {

  @Column(nullable = false)
  @Getter
  @Setter
  private Boolean openLmisField;

  @Getter
  @Setter
  private String dataFieldLabel;

  @Getter
  @Setter
  private String columnLabel;

  @Column(nullable = false)
  @Getter
  @Setter
  private Boolean include;

  @Column(nullable = false)
  @Getter
  @Setter
  private Integer position;

  @Getter
  @Setter
  private String format;

  @Getter
  @Setter
  private String nested;

  @Getter
  @Setter
  private String keyPath;

  @Getter
  @Setter
  private String related;

  @Getter
  @Setter
  private String relatedKeyPath;

  @ManyToOne(cascade = CascadeType.REFRESH)
  @JoinColumn(name = "orderFileTemplateId", nullable = false)
  @Getter
  @Setter
  private OrderFileTemplate orderFileTemplate;
}
