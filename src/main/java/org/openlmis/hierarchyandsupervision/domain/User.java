package org.openlmis.hierarchyandsupervision.domain;

import com.fasterxml.jackson.annotation.JsonView;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.referencedata.domain.BaseEntity;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.view.View;

import java.util.List;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.PrePersist;
import javax.persistence.Table;

@SuppressWarnings("PMD.UnusedPrivateField")
@Entity
@Table(name = "users", schema = "referencedata")
@NoArgsConstructor
public class User extends BaseEntity {
  private static final String DEFAULT_PASSWORD = "not-in-use";

  @JsonView(View.BasicInformation.class)
  @Column(nullable = false, unique = true, columnDefinition = "text")
  @Getter
  @Setter
  private String username;

  @Column(nullable = false, columnDefinition = "text DEFAULT '" + DEFAULT_PASSWORD + "'::text")
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
  @JoinColumn(name = "supervisoryNodeId")
  @Getter
  @Setter
  private SupervisoryNode supervisedNode; //TODO Role based access control for Requisitions

  @ManyToOne
  @JoinColumn(name = "facilityid")
  @Getter
  @Setter
  private Facility homeFacility;

  @Column(nullable = false, columnDefinition = "boolean DEFAULT false")
  @Getter
  @Setter
  private Boolean verified;

  @Column(nullable = false, columnDefinition = "boolean DEFAULT false")
  @Getter
  @Setter
  private Boolean active;

  @OneToMany
  @JoinColumn(name = "roleId")
  @Getter
  @Setter
  private List<Role> roles;

  @PrePersist
  private void prePersist() {
    if (this.verified == null) {
      this.verified = false;
    }

    if (this.active == null) {
      this.active = false;
    }

    if (this.password == null) {
      this.password = DEFAULT_PASSWORD;
    }
  }
}
