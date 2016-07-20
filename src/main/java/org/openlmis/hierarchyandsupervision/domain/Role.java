package org.openlmis.hierarchyandsupervision.domain;


import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.referencedata.domain.BaseEntity;

import java.util.List;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.Table;

@Entity
@Table(name = "roles", schema = "referencedata")
@NoArgsConstructor
public class Role extends BaseEntity {
  private final String text = "text";

  @Column(nullable = false, unique = true, columnDefinition = text)
  @Getter
  @Setter
  private String name;

  @Column(columnDefinition = text)
  @Getter
  @Setter
  private String description;

  @OneToMany
  @JoinColumn(name = "rightId")
  @Getter
  @Setter
  private List<Right> rights;
}
