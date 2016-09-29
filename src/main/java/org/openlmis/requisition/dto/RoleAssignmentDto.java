package org.openlmis.requisition.dto;

import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

@Getter
@Setter
public class RoleAssignmentDto {
  private UUID id;
  protected RoleDto role;
  protected UserDto user;
}
