package org.openlmis.requisition.dto;

import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@AllArgsConstructor
public class UserDto {

  @Getter
  @Setter
  private UUID id;

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

  public SupervisoryNodeDto getSupervisedNode() {
    //TODO in Sprint 9
    return null;
  }
}
