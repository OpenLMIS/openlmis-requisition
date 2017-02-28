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

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.javers.common.collections.Optional;
import org.javers.core.Javers;
import org.javers.core.commit.CommitMetadata;
import org.javers.core.diff.Change;
import org.javers.repository.jql.JqlQuery;
import org.joda.time.LocalDateTime;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;

import java.util.Collections;

@SuppressWarnings({"PMD.UnusedPrivateField"})
@RunWith(MockitoJUnitRunner.class)
public class RequisitionStatusProcessorTest {

  @Mock
  private Javers javers;

  @Mock
  private ConvertToOrderNotifier convertToOrderNotifier;

  @Mock
  private ApprovalNotifier approvalNotifier;

  @Mock
  private RequisitionStatusNotifier requisitionStatusNotifier;

  @InjectMocks
  private DefaultRequisitionStatusProcessor requisitionStatusProcessor;

  @Test
  public void shouldNotifyConvertToOrder() {
    Requisition requisition = mock(Requisition.class);
    when(requisition.getStatus()).thenReturn(RequisitionStatus.RELEASED);
    mockChange();

    requisitionStatusProcessor.statusChange(requisition);

    verify(convertToOrderNotifier).notifyConvertToOrder(eq(requisition));
  }

  @Test
  public void shouldNotifyApproversWhenAuthorized() {
    Requisition requisition = mock(Requisition.class);
    when(requisition.isApprovable()).thenReturn(true);
    mockChange();

    requisitionStatusProcessor.statusChange(requisition);

    verify(approvalNotifier).notifyApprovers(eq(requisition));
  }

  @Test
  public void shouldNotifyApproversWhenInApproval() {
    Requisition requisition = mock(Requisition.class);
    when(requisition.isApprovable()).thenReturn(true);
    mockChange();

    requisitionStatusProcessor.statusChange(requisition);

    verify(approvalNotifier).notifyApprovers(eq(requisition));
  }

  private void mockChange() {
    Change change = mock(Change.class);

    when(javers.findChanges(any(JqlQuery.class)))
        .thenReturn(Collections.singletonList(change));

    CommitMetadata commitMetadata = mock(CommitMetadata.class);
    when(commitMetadata.getCommitDate()).thenReturn(LocalDateTime.now());
    when(change.getCommitMetadata()).thenReturn(Optional.of(commitMetadata));
  }

}
