package org.openlmis.referencedata.domain;

import lombok.*;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/*
 * ProductCategory represents the category for product. Also defines the contract for creation/upload of ProductCategory like
 * code, name and displayOrder are mandatory.
 */

@Entity
@Table(name = "program_categories")
@NoArgsConstructor
@AllArgsConstructor
public class ProductCategory extends BaseEntity {

  @Column(nullable = false, unique = true)
  @Getter
  @Setter
  private String code;

  @Column(nullable = false, unique = true)
  @Getter
  @Setter
  private String name;

  @Column(nullable = false)
  @Getter
  @Setter
  private Integer displayOrder;
}