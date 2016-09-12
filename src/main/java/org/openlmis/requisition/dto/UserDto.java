package org.openlmis.requisition.dto;

import javax.persistence.Entity;
import javax.persistence.Table;
import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.openlmis.referencedata.domain.BaseEntity;

import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "user_dto", schema = "referencedata")
@NoArgsConstructor
@AllArgsConstructor
public class UserDto extends BaseEntity {

  @Getter
  @Setter
  private String username;

  @Getter
  @Setter
  private String firstName;

  @Getter
  @Setter
  private String lastName;

  @Getter
  @Setter
  private String email;

  @Setter
  private UUID supervisedNode;

  @Getter
  @Setter
  private String timezone;

  @Getter
  @Setter
  private UUID homeFacility;

  @Getter
  @Setter
  private boolean verified;

  @Getter
  @Setter
  private boolean active;

  @Getter
  private UUID roleAssignments;

  public SupervisoryNode getSupervisedNode() {
    return null;
  }

}
