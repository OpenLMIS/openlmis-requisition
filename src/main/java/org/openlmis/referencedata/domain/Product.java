package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "products")
@NoArgsConstructor
public class Product {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  @Getter
  @Setter
  private Integer id;

  @Column(nullable = false, unique = true, columnDefinition = "text")
  @Getter
  @Setter
  private String code;

  @Column(nullable = false, columnDefinition = "text")
  @Getter
  @Setter
  private String primaryName;

  @Column(nullable = false, columnDefinition = "text")
  @Getter
  @Setter
  private String dispensingUnit;

  @Column(nullable = false)
  @Getter
  @Setter
  private Integer dosesPerDispensingUnit;

  @Column(nullable = false)
  @Getter
  @Setter
  private Integer packSize;

  @Column(nullable = false)
  @Getter
  @Setter
  private Integer packRoundingThreshold;

  @Column(nullable = false)
  @Getter
  @Setter
  private Boolean roundToZero;

  @Column(nullable = false)
  @Getter
  @Setter
  private Boolean active;

  @Column(nullable = false)
  @Getter
  @Setter
  private Boolean fullSupply;

  @Column(nullable = false)
  @Getter
  @Setter
  private Boolean tracer;
}
