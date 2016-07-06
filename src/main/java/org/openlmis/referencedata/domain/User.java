package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

@SuppressWarnings("PMD.UnusedPrivateField")
@Entity
@Table(name = "users")
@NoArgsConstructor
public class User extends BaseEntity {

  @Column(nullable = false, columnDefinition = "text")
  @Getter
  @Setter
  private String username;

  @Column(nullable = false, columnDefinition = "text DEFAULT 'not-in-use'::text")
  @Setter
  private String password;

  @Column(nullable = false, columnDefinition = "text")
  @Getter
  @Setter
  private String firstName;

  @Column(nullable = false, columnDefinition = "text")
  @Getter
  @Setter
  private String lastName;

  @ManyToOne
  @JoinColumn(name = "facilityid")
  private Facility homeFacility;

  @Column(columnDefinition = "boolean DEFAULT false")
  @Getter
  @Setter
  private Boolean verified;

  @Column(columnDefinition = "boolean DEFAULT false")
  @Getter
  @Setter
  private Boolean active;

  @OneToMany
  @JoinColumn(name = "roleid")
  @Getter
  @Setter
  private List<Role> roles;
}
