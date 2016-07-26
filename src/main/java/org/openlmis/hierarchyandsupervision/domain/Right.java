package org.openlmis.hierarchyandsupervision.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.referencedata.domain.BaseEntity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "rights", schema = "referencedata")
@NoArgsConstructor
public class Right extends BaseEntity {
  private final String text = "text";

  @Column(nullable = false, unique = true, columnDefinition = text)
  @Getter
  @Setter
  private String name;

  @Column(nullable = false, columnDefinition = text)
  @Getter
  @Setter
  private String rightType;

  @Column(columnDefinition = text)
  @Getter
  @Setter
  private String description;
}
