package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "rights")
@NoArgsConstructor
public class Right extends BaseEntity {

  public static final String TEXT = "text";

  @Column(nullable = false, unique = true, columnDefinition = TEXT)
  @Getter
  @Setter
  private String name;

  @Column(nullable = false, columnDefinition = TEXT)
  @Getter
  @Setter
  private String rightType;

  @Column(columnDefinition = TEXT)
  @Getter
  @Setter
  private String description;
}
