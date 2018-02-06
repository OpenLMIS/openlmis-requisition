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

import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.google.common.collect.Sets;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.dto.DetailedRoleAssignmentDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.RoleDto;
import org.openlmis.requisition.testutils.DtoGenerator;

import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

@RunWith(MockitoJUnitRunner.class)
public class UserRoleAssignmentsReferenceDataServiceTest {

  @Mock
  private UserRoleAssignmentsReferenceDataService roleService;

  private RightDto approveRequisitionRight = DtoGenerator.of(RightDto.class, 2).get(0);
  private RightDto convertToOrderRight = DtoGenerator.of(RightDto.class, 2).get(1);
  private RoleDto role = DtoGenerator.of(RoleDto.class);

  private UUID supervisoryNodeId = UUID.randomUUID();
  private UUID programId = UUID.randomUUID();
  private UUID userId = UUID.randomUUID();

  @Before
  public void setUp() {
    when(roleService.hasSupervisionRight(
        any(RightDto.class),
        any(UUID.class),
        any(UUID.class),
        any(UUID.class))).thenCallRealMethod();
  }

  @Test
  public void shouldReturnTrueIfUserCanApproveRequisition() {
    mockSupervisionRoleAssignment(programId, supervisoryNodeId,
        approveRequisitionRight);

    assertTrue(roleService.hasSupervisionRight(approveRequisitionRight, userId,
        programId, supervisoryNodeId));
  }

  @Test
  public void shouldReturnFalseIfUserHasNoApproveRigthAssigned() {
    mockSupervisionRoleAssignment(programId, supervisoryNodeId, convertToOrderRight);

    assertFalse(roleService.hasSupervisionRight(approveRequisitionRight, userId,
        programId, supervisoryNodeId));
  }

  @Test
  public void shouldReturnFalseIfUserHasDifferentSupervisoryNodeAssignedToApproveRight() {
    mockSupervisionRoleAssignment(programId, UUID.randomUUID(),
        approveRequisitionRight);

    assertFalse(roleService.hasSupervisionRight(approveRequisitionRight, userId,
        programId, supervisoryNodeId));
  }

  @Test
  public void shouldReturnFalseIfUserHasDifferentProgramAssignedToApproveRight() {
    mockSupervisionRoleAssignment(UUID.randomUUID(), supervisoryNodeId,
        approveRequisitionRight);

    assertFalse(roleService.hasSupervisionRight(approveRequisitionRight, userId,
        programId, supervisoryNodeId));
  }

  @Test
  public void shouldReturnFalseIfUserOrRightIsNotGiven() {
    assertFalse(roleService.hasSupervisionRight(null, userId, programId, supervisoryNodeId));
    assertFalse(roleService.hasSupervisionRight(approveRequisitionRight, null,
        programId, supervisoryNodeId));
  }

  private void mockSupervisionRoleAssignment(UUID program, UUID node,
                                                          RightDto right) {
    DetailedRoleAssignmentDto detailedRoleAssignmentDto = mock(DetailedRoleAssignmentDto.class);
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(program);
    when(detailedRoleAssignmentDto.getSupervisoryNodeId()).thenReturn(node);
    when(detailedRoleAssignmentDto.getRole()).thenReturn(role);

    Set<RightDto> rights = new HashSet<>();
    rights.add(right);
    role.setRights(rights);

    when(roleService.getRoleAssignments(userId))
        .thenReturn(Sets.newHashSet(detailedRoleAssignmentDto));
  }
}
