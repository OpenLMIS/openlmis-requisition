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

package org.openlmis.requisition.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;

import java.util.UUID;

@SuppressWarnings({"PMD.UnusedPrivateField"})
@RunWith(MockitoJUnitRunner.class)
public class RequisitionStatusProcessorTest {

  @Mock
  private ConvertToOrderNotifier convertToOrderNotifier;

  @Mock
  private ApprovalNotifier approvalNotifier;

  @Mock
  private RequisitionStatusNotifier requisitionStatusNotifier;

  @Mock
  private ApprovedRequisitionNotifier approvedRequisitionNotifier;

  @Mock
  private SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

  @Mock
  private SupervisoryNodeDto supervisoryNodeDto;

  private UUID supervisoryNodeId = UUID.randomUUID();

  @InjectMocks
  private DefaultRequisitionStatusProcessor requisitionStatusProcessor;

  @Before
  public void setUp() {
    doReturn(supervisoryNodeDto).when(supervisoryNodeReferenceDataService)
        .findSupervisoryNode(any(UUID.class), any(UUID.class));
    doReturn(supervisoryNodeId).when(supervisoryNodeDto).getId();
  }

  @Test
  public void shouldNotifyConvertToOrder() {
    Requisition requisition = mock(Requisition.class);
    when(requisition.getStatus()).thenReturn(RequisitionStatus.RELEASED);

    requisitionStatusProcessor.statusChange(requisition);

    verify(convertToOrderNotifier).notifyConvertToOrder(eq(requisition));
  }

  @Test
  public void shouldNotNotifyForRequisitionStatusWhenPreAuthorized() {
    Requisition requisition = mock(Requisition.class);
    when(requisition.isPreAuthorize()).thenReturn(true);

    requisitionStatusProcessor.statusChange(requisition);

    verify(requisitionStatusNotifier, never()).notifyStatusChanged(eq(requisition));
  }

  @Test
  public void shouldNotifyForRequisitionStatusWhenAuthorized() {
    Requisition requisition = mock(Requisition.class);
    when(requisition.isPreAuthorize()).thenReturn(false);

    requisitionStatusProcessor.statusChange(requisition);

    verify(requisitionStatusNotifier).notifyStatusChanged(eq(requisition));
  }

  @Test
  public void shouldNotifyApprovers() {
    Requisition requisition = mock(Requisition.class);
    when(requisition.isApprovable()).thenReturn(true);

    requisitionStatusProcessor.statusChange(requisition);

    verify(approvalNotifier).notifyApprovers(eq(requisition));
  }

  @Test
  public void shouldNotifyClerks() {
    Requisition requisition = mock(Requisition.class);
    when(requisition.getStatus()).thenReturn(RequisitionStatus.APPROVED);

    requisitionStatusProcessor.statusChange(requisition);

    verify(approvedRequisitionNotifier).notifyClerks(requisition);
  }

  @Test
  public void shouldAssignInitialSupervisoryNodeToRequisition() {
    Requisition requisition = new Requisition();
    requisition.setStatus(RequisitionStatus.AUTHORIZED);

    requisitionStatusProcessor.statusChange(requisition);

    assertEquals(supervisoryNodeId, requisition.getSupervisoryNodeId());
  }

  @Test
  public void shouldNotOverwriteSupervisoryNodeWhenOneIsAssigned() {
    Requisition requisition = new Requisition();
    requisition.setStatus(RequisitionStatus.AUTHORIZED);
    UUID assignedSupervisoryNode = UUID.randomUUID();
    requisition.setSupervisoryNodeId(assignedSupervisoryNode);

    requisitionStatusProcessor.statusChange(requisition);

    verify(supervisoryNodeReferenceDataService, never())
        .findSupervisoryNode(any(UUID.class), any(UUID.class));
    assertEquals(assignedSupervisoryNode, requisition.getSupervisoryNodeId());
  }

  @Test
  public void shouldNotAssignSupervisoryNodeWhenRequisitionIsNotReadyForApproval() {
    Requisition requisition = new Requisition();
    requisition.setStatus(RequisitionStatus.SUBMITTED);

    requisitionStatusProcessor.statusChange(requisition);

    verify(supervisoryNodeReferenceDataService, never())
        .findSupervisoryNode(any(UUID.class), any(UUID.class));
    assertNull(requisition.getSupervisoryNodeId());
  }
}
