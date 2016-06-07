package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "facilities")
@NoArgsConstructor
public class Facility {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  @Getter
  @Setter
  private Integer id;

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

  @Column(columnDefinition = "text")
  @Getter
  @Setter
  private String comment;

  @Column(nullable = false)
  @Getter
  @Setter
  private Boolean enabled;
}
