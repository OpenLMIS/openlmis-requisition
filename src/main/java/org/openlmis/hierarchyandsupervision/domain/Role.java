package org.openlmis.hierarchyandsupervision.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.referencedata.domain.BaseEntity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import java.util.List;

@Entity
@Table(name = "roles", schema = "referencedata")
@NoArgsConstructor
public class Role extends BaseEntity {
  private static final String TEXT = "text";

  @Column(nullable = false, unique = true, columnDefinition = TEXT)
  @Getter
  @Setter
  private String name;

  @Column(columnDefinition = TEXT)
  @Getter
  @Setter
  private String description;

  @OneToMany
  @JoinColumn(name = "rightId")
  @Getter
  @Setter
  private List<Right> rights;

  /**
   * Copy values of attributes into new or updated Role.
   *
   * @param role Role with new values.
   */
  public void updateFrom(Role role) {
    this.name = role.getName();
    this.description = role.getDescription();
    this.rights = role.getRights();
  }
}
