package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "facility_operators", schema = "referencedata")
@NoArgsConstructor
public class FacilityOperator extends BaseEntity {

  @Column(nullable = false, unique = true, columnDefinition = "text")
  @Getter
  @Setter
  private String code;

  @Column(columnDefinition = "text")
  @Getter
  @Setter
  private String name;

  @Column(columnDefinition = "text")
  @Getter
  @Setter
  private String description;

  @Getter
  @Setter
  private Integer displayOrder;

  /**
   * Copy values of attributes into new or updated FacilityOperator.
   *
   * @param facilityOperator FacilityOperator with new values.
   */
  public void updateFrom(FacilityOperator facilityOperator) {
    this.code = facilityOperator.getCode();
    this.name = facilityOperator.getName();
    this.description = facilityOperator.getDescription();
    this.displayOrder = facilityOperator.getDisplayOrder();
  }
}
