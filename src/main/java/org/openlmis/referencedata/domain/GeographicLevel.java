package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "geographic_levels", schema = "referencedata")
@NoArgsConstructor
public class GeographicLevel extends BaseEntity {

  @Column(nullable = false, unique = true, columnDefinition = "text")
  @Getter
  @Setter
  private String code;

  @Column(columnDefinition = "text")
  @Getter
  @Setter
  private String name;

  @Column(nullable = false)
  @Getter
  @Setter
  private Integer levelNumber;

  /**
   * Copy values of attributes into new or updated GeographicLevel.
   *
   * @param geographicLevel GeographicLevel with new values.
   */
  public void updateFrom(GeographicLevel geographicLevel) {
    this.code = geographicLevel.getCode();
    this.name = geographicLevel.getName();
    this.levelNumber = geographicLevel.getLevelNumber();
  }
}
