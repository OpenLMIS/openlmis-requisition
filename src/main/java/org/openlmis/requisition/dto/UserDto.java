package org.openlmis.requisition.dto;

import java.util.Set;
import java.util.UUID;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class UserDto {
  private UUID id;
  private String username;
  private String firstName;
  private String lastName;
  private String email;
  private boolean verified;
  private FacilityDto homeFacility;
  private Set<ProgramDto> homeFacilityPrograms;
  private Set<ProgramDto> supervisedPrograms;
  private Set<FacilityDto> supervisedFacilities;
  private Set<FacilityDto> fulfillmentFacilities;
  private Set<RoleAssignmentDto> roleAssignments;
}
