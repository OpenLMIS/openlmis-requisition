/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org.
 */

package org.openlmis.requisition.service.referencedata;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.openlmis.requisition.dto.DetailedRoleAssignmentDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class UserRoleAssignmentsReferenceDataService extends
    BaseReferenceDataService<DetailedRoleAssignmentDto> {

  @Autowired
  private SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

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

  /**
   * Checks if given user has supervision right with given parameters.
   *
   * @param right             right to be checked
   * @param userId            UUID of user that roles will be verified
   * @param programId         UUID of program assigned to role
   * @param facilityId        UUID of facility assigned to role
   * @param supervisoryNodeId UUID of supervisory node assigned to role
   */
  public boolean hasSupervisionRight(RightDto right, UUID userId, UUID programId, UUID facilityId,
                                     UUID supervisoryNodeId) {
    if (userId == null || right == null) {
      return false;
    }

    Set<UUID> facilitySupervisoryNodeIds = supervisoryNodeReferenceDataService
        .findByFacility(facilityId)
        .stream()
        .map(SupervisoryNodeDto::getId)
        .collect(Collectors.toSet());

    return hasSupervisionRight(right, getRoleAssignments(userId), programId,
        facilitySupervisoryNodeIds, supervisoryNodeId);
  }

  /**
   * Checks the same rule as {@link #hasSupervisionRight(RightDto, UUID, UUID, UUID, UUID)} over
   * inputs the caller has already fetched, so that a batch can fetch them once.
   *
   * @param right                      right to be checked
   * @param roleAssignments            the user's role assignments
   * @param programId                  UUID of program assigned to role
   * @param facilitySupervisoryNodeIds UUIDs of the nodes whose requisition group contains the
   *                                   facility; null counts as none
   * @param supervisoryNodeId          UUID of supervisory node assigned to role
   */
  public static boolean hasSupervisionRight(RightDto right,
      Collection<DetailedRoleAssignmentDto> roleAssignments, UUID programId,
      Set<UUID> facilitySupervisoryNodeIds, UUID supervisoryNodeId) {
    if (right == null || roleAssignments == null) {
      return false;
    }

    Set<UUID> nodeIds = facilitySupervisoryNodeIds == null
        ? Collections.emptySet()
        : facilitySupervisoryNodeIds;

    return roleAssignments.stream()
        .filter(r -> r.getRole().getRights().contains(right))
        .anyMatch(r -> hasAnySupervisionRoleWithGivenParameters(r, programId,
           nodeIds, supervisoryNodeId));
  }

  private static boolean hasAnySupervisionRoleWithGivenParameters(DetailedRoleAssignmentDto role,
                                                           UUID programId,
                                                           Set<UUID> facilitySupervisoryNodeIds,
                                                           UUID supervisoryNodeId) {
    return roleHasSupervisoryNodeIdAndProgramId(role)
        && roleHasProgramId(role, programId)
        && roleHasSupervisoryNodeId(role, supervisoryNodeId, facilitySupervisoryNodeIds);
  }

  private static boolean roleHasSupervisoryNodeIdAndProgramId(DetailedRoleAssignmentDto role) {
    return role.getSupervisoryNodeId() != null
            && role.getProgramId() != null;
  }

  private static boolean roleHasProgramId(DetailedRoleAssignmentDto role,  UUID programId) {
    return programId == null || programId.equals(role.getProgramId());
  }

  private static boolean roleHasSupervisoryNodeId(DetailedRoleAssignmentDto role,
      UUID supervisoryNodeId, Set<UUID> facilitySupervisoryNodeIds) {
    return supervisoryNodeId == null
            || facilitySupervisoryNodeIds.contains(role.getSupervisoryNodeId())
            || supervisoryNodeId.equals(role.getSupervisoryNodeId());
  }
}
