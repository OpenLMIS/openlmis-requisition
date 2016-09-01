package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "facility_types", schema = "referencedata")
@NoArgsConstructor
public class FacilityType extends BaseEntity {

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

  @Getter
  @Setter
  private Boolean active;

  /**
   * Copy values of attributes into new or updated FacilityType.
   *
   * @param facilityType FacilityType with new values.
   */
  public void updateFrom(FacilityType facilityType) {
    this.code = facilityType.getCode();
    this.name = facilityType.getName();
    this.description = facilityType.getDescription();
    this.displayOrder = facilityType.getDisplayOrder();
  }
}
