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

import static java.util.Collections.singletonList;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.common.collect.Sets;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.openlmis.requisition.dto.DetailedRoleAssignmentDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.RoleDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.testutils.DetailedRoleAssignmentDtoDataBuilder;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.testutils.RoleDtoDataBuilder;
import org.openlmis.requisition.testutils.SupervisoryNodeDtoDataBuilder;
import org.springframework.test.util.ReflectionTestUtils;

@SuppressWarnings("PMD.TooManyMethods")
public class UserRoleAssignmentsReferenceDataServiceTest
    extends BaseReferenceDataServiceTest<DetailedRoleAssignmentDto> {

  @Mock
  private SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

  private UserRoleAssignmentsReferenceDataService service;

  @Override
  protected DetailedRoleAssignmentDto generateInstance() {
    return new DetailedRoleAssignmentDtoDataBuilder().buildAsDto();
  }

  @Override
  protected BaseReferenceDataService<DetailedRoleAssignmentDto> getService() {
    return new UserRoleAssignmentsReferenceDataService();
  }

  @Override
  @Before
  public void setUp() {
    super.setUp();
    service = (UserRoleAssignmentsReferenceDataService) prepareService();
    ReflectionTestUtils.setField(service, "supervisoryNodeReferenceDataService",
        supervisoryNodeReferenceDataService);
    when(supervisoryNodeReferenceDataService.findByFacility(facilityId))
        .thenReturn(Collections.singletonList(supervisoryNode));
  }

  private RightDto approveRequisitionRight = DtoGenerator.of(RightDto.class, 2).get(0);
  private RightDto convertToOrderRight = DtoGenerator.of(RightDto.class, 2).get(1);
  private RoleDto role = DtoGenerator.of(RoleDto.class);

  private SupervisoryNodeDto supervisoryNode = DtoGenerator.of(SupervisoryNodeDto.class);

  private UUID supervisoryNodeId = UUID.randomUUID();
  private UUID programId = UUID.randomUUID();
  private UUID userId = UUID.randomUUID();
  private UUID facilityId = UUID.randomUUID();

  @Test
  public void shouldGetRoleAssignmentsForUser() {
    // given
    UUID userId = UUID.randomUUID();

    // when
    DetailedRoleAssignmentDto dto = mockArrayResponseEntityAndGetDto();
    Collection<DetailedRoleAssignmentDto> result = service.getRoleAssignments(userId);

    // then
    assertThat(result, hasSize(1));
    assertTrue(result.contains(dto));

    verifyArrayRequest()
        .isGetRequest()
        .hasAuthHeader()
        .hasEmptyBody()
        .isUriStartsWith(service.getServiceUrl() + service.getUrl() + userId + "/roleAssignments");
  }

  @Test
  public void shouldReturnTrueIfUserCanApproveRequisition() {
    mockSupervisionRoleAssignment(programId, supervisoryNode.getId(),
            approveRequisitionRight);

    assertTrue(service.hasSupervisionRight(approveRequisitionRight, userId,
        programId, facilityId, supervisoryNodeId));
  }

  @Test
  public void shouldReturnFalseIfUserHasNoApproveRightAssigned() {
    mockSupervisionRoleAssignment(programId, supervisoryNodeId,
            convertToOrderRight);

    assertFalse(service.hasSupervisionRight(approveRequisitionRight, userId,
        programId, facilityId, supervisoryNodeId));
  }

  @Test
  public void shouldReturnFalseIfUserHasDifferentSupervisoryNodeAssignedToApproveRight() {
    mockSupervisionRoleAssignment(programId, UUID.randomUUID(),
            approveRequisitionRight);

    assertFalse(service.hasSupervisionRight(approveRequisitionRight, userId,
        programId, facilityId, supervisoryNodeId));
  }

  @Test
  public void shouldReturnFalseIfUserHasDifferentProgramAssignedToApproveRight() {
    mockSupervisionRoleAssignment(UUID.randomUUID(), supervisoryNodeId,
        approveRequisitionRight);

    assertFalse(service.hasSupervisionRight(approveRequisitionRight, userId,
        programId, facilityId, supervisoryNodeId));
  }

  @Test
  public void shouldReturnFalseIfUserOrRightIsNotGiven() {
    disableAuthCheck();
    assertFalse(service.hasSupervisionRight(null, userId, programId, facilityId,
        supervisoryNodeId));
    assertFalse(service.hasSupervisionRight(approveRequisitionRight, null,
        programId, facilityId, supervisoryNodeId));
  }

  @Test
  public void shouldAcceptRoleAtAnyNodeSupervisingTheFacility() {
    SupervisoryNodeDto otherNode = new SupervisoryNodeDtoDataBuilder().buildAsDto();
    when(supervisoryNodeReferenceDataService.findByFacility(facilityId))
        .thenReturn(Arrays.asList(supervisoryNode, otherNode));
    mockSupervisionRoleAssignment(programId, otherNode.getId(), approveRequisitionRight);

    assertTrue(service.hasSupervisionRight(approveRequisitionRight, userId,
        programId, facilityId, supervisoryNodeId));
    verify(supervisoryNodeReferenceDataService).findByFacility(facilityId);
  }

  @Test
  public void shouldAcceptPrefetchedRoleAtRequisitionNode() {
    disableAuthCheck();
    assertTrue(UserRoleAssignmentsReferenceDataService.hasSupervisionRight(approveRequisitionRight,
        singletonList(roleAssignment(programId, supervisoryNodeId, approveRequisitionRight)),
        programId, Collections.emptySet(), supervisoryNodeId));
    verify(supervisoryNodeReferenceDataService, never()).findByFacility(any(UUID.class));
  }

  @Test
  public void shouldAcceptPrefetchedRoleAtNodeSupervisingTheFacility() {
    disableAuthCheck();
    UUID facilityNodeId = UUID.randomUUID();
    assertTrue(UserRoleAssignmentsReferenceDataService.hasSupervisionRight(approveRequisitionRight,
        singletonList(roleAssignment(programId, facilityNodeId, approveRequisitionRight)),
        programId, Sets.newHashSet(facilityNodeId), supervisoryNodeId));
  }

  @Test
  public void shouldAcceptPrefetchedRoleWhenRequisitionHasNoSupervisoryNode() {
    disableAuthCheck();
    assertTrue(UserRoleAssignmentsReferenceDataService.hasSupervisionRight(approveRequisitionRight,
        singletonList(roleAssignment(programId, UUID.randomUUID(), approveRequisitionRight)),
        programId, Collections.emptySet(), null));
  }

  @Test
  public void shouldRefusePrefetchedRoleAtUnrelatedNode() {
    disableAuthCheck();
    assertFalse(UserRoleAssignmentsReferenceDataService.hasSupervisionRight(approveRequisitionRight,
        singletonList(roleAssignment(programId, UUID.randomUUID(), approveRequisitionRight)),
        programId, Sets.newHashSet(UUID.randomUUID()), supervisoryNodeId));
  }

  @Test
  public void shouldRefusePrefetchedHomeFacilityRole() {
    disableAuthCheck();
    assertFalse(UserRoleAssignmentsReferenceDataService.hasSupervisionRight(approveRequisitionRight,
        singletonList(roleAssignment(programId, null, approveRequisitionRight)),
        programId, Sets.newHashSet(supervisoryNodeId), supervisoryNodeId));
  }

  @Test
  public void shouldRefusePrefetchedRoleWithoutTheRightOrProgram() {
    disableAuthCheck();
    assertFalse(UserRoleAssignmentsReferenceDataService.hasSupervisionRight(approveRequisitionRight,
        singletonList(roleAssignment(programId, supervisoryNodeId, convertToOrderRight)),
        programId, Collections.emptySet(), supervisoryNodeId));
    assertFalse(UserRoleAssignmentsReferenceDataService.hasSupervisionRight(approveRequisitionRight,
        singletonList(roleAssignment(UUID.randomUUID(), supervisoryNodeId,
            approveRequisitionRight)),
        programId, Collections.emptySet(), supervisoryNodeId));
  }

  @Test
  public void shouldRefusePrefetchedWhenInputsAreMissing() {
    disableAuthCheck();
    List<DetailedRoleAssignmentDto> assignments =
        singletonList(roleAssignment(programId, supervisoryNodeId, approveRequisitionRight));
    assertFalse(UserRoleAssignmentsReferenceDataService.hasSupervisionRight(null, assignments,
        programId, Collections.emptySet(), supervisoryNodeId));
    assertFalse(UserRoleAssignmentsReferenceDataService.hasSupervisionRight(
        approveRequisitionRight, null, programId, Collections.emptySet(), supervisoryNodeId));
    assertFalse(UserRoleAssignmentsReferenceDataService.hasSupervisionRight(
        approveRequisitionRight, Collections.emptyList(), programId, Collections.emptySet(),
        supervisoryNodeId));
    assertTrue(UserRoleAssignmentsReferenceDataService.hasSupervisionRight(
        approveRequisitionRight, assignments, programId, null, supervisoryNodeId));
  }

  private DetailedRoleAssignmentDto roleAssignment(UUID program, UUID node, RightDto right) {
    RoleDto assignedRole = new RoleDtoDataBuilder().buildAsDto();
    assignedRole.setRights(Sets.newHashSet(right));
    DetailedRoleAssignmentDto assignment = new DetailedRoleAssignmentDtoDataBuilder().buildAsDto();
    assignment.setRole(assignedRole);
    assignment.setProgramId(program);
    assignment.setSupervisoryNodeId(node);
    return assignment;
  }

  private void mockSupervisionRoleAssignment(UUID program, UUID node, RightDto right) {
    DetailedRoleAssignmentDto detailedRoleAssignmentDto = mock(DetailedRoleAssignmentDto.class);
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(program);
    when(detailedRoleAssignmentDto.getSupervisoryNodeId()).thenReturn(node);
    when(detailedRoleAssignmentDto.getRole()).thenReturn(role);

    role.setRights(Sets.newHashSet(right));

    mockArrayResponseEntity(detailedRoleAssignmentDto);
  }
}
