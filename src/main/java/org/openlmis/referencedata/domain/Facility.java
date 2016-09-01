package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import java.util.Date;
import java.util.List;

@Entity
@Table(name = "facilities", schema = "referencedata")
@NoArgsConstructor
public class Facility extends BaseEntity {

  public static final String TEXT = "text";

  @Column(nullable = false, unique = true, columnDefinition = TEXT)
  @Getter
  @Setter
  private String code;

  @Column(columnDefinition = TEXT)
  @Getter
  @Setter
  private String name;

  @Column(columnDefinition = TEXT)
  @Getter
  @Setter
  private String description;

  @ManyToOne
  @JoinColumn(name = "geographiczoneid", nullable = false)
  @Getter
  @Setter
  private GeographicZone geographicZone;

  @ManyToOne
  @JoinColumn(name = "typeid", nullable = false)
  @Getter
  @Setter
  private FacilityType type;

  @ManyToOne
  @JoinColumn(name = "operatedbyid")
  @Getter
  @Setter
  private FacilityOperator operator;

  @Column(nullable = false)
  @Getter
  @Setter
  private Boolean active;

  @Getter
  @Setter
  private Date goLiveDate;

  @Getter
  @Setter
  private Date goDownDate;

  @Column(columnDefinition = TEXT)
  @Getter
  @Setter
  private String comment;

  @Column(nullable = false)
  @Getter
  @Setter
  private Boolean enabled;

  @Getter
  @Setter
  private Boolean openLmisAccessible;

  @OneToMany
  @JoinColumn(name = "programId")
  @Getter
  @Setter
  private List<Program> supportedPrograms;

  /**
   * Copy values of attributes into new or updated Facility.
   *
   * @param facility Facility with new values.
   */
  public void updateFrom(Facility facility) {
    this.code = facility.getCode();
    this.name = facility.getName();
    this.description = facility.getDescription();
    this.geographicZone = facility.getGeographicZone();
    this.type = facility.getType();
    this.operator = facility.getOperator();
    this.active = facility.getActive();
    this.goLiveDate = facility.getGoLiveDate();
    this.goDownDate = facility.getGoDownDate();
    this.comment = facility.getComment();
    this.enabled = facility.getEnabled();
    this.openLmisAccessible = facility.getOpenLmisAccessible();
    this.supportedPrograms = facility.getSupportedPrograms();
  }
}
