package org.openlmis.product.domain;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.referencedata.domain.BaseEntity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/*
 * ProductCategory represents the category for product. Also defines the contract for 
 * creation/upload of ProductCategory like code, name and displayOrder are mandatory.
 */

@Entity
@Table(name = "product_categories", schema = "referencedata")
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

  /**
   * Copy values of attributes into new or updated ProductCategory.
   *
   * @param productCategory ProductCategory with new values.
   */
  public void updateFrom(ProductCategory productCategory) {
    this.code = productCategory.getCode();
    this.name = productCategory.getName();
    this.displayOrder = productCategory.getDisplayOrder();
  }
}