package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.DetailedRoleAssignmentDto;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.UUID;

@Service
public class UserRoleAssignmentsReferenceDataService extends
    BaseReferenceDataService<DetailedRoleAssignmentDto> {

  @Override
  protected String getUrl() {
    return "/api/users/";
  }

  @Override
  protected Class<DetailedRoleAssignmentDto> getResultClass() {
    return DetailedRoleAssignmentDto.class;
  }

  @Override
  protected Class<DetailedRoleAssignmentDto[]> getArrayResultClass() {
    return DetailedRoleAssignmentDto[].class;
  }

  public Collection<DetailedRoleAssignmentDto> getRoleAssignments(UUID userId) {
    return findAll(userId + "/roleAssignments");
  }
}
