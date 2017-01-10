package org.openlmis.requisition.dto;

import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

@JsonInclude(JsonInclude.Include.NON_NULL)
@Getter
@Setter
public class DetailedRoleAssignmentDto {
  private RoleDto role;
  private String programCode;
  private String supervisoryNodeCode;
  private String warehouseCode;
  private UUID programId;
  private UUID supervisoryNodeId;
}