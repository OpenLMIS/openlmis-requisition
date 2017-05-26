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

import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.contains;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.refEq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_ACTION_REQUIRED_CONTENT;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_ACTION_REQUIRED_SUBJECT;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusChange;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.RightReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisingUsersReferenceDataService;
import org.openlmis.utils.Message;
import org.openlmis.utils.RightName;
import java.lang.reflect.Field;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

@RunWith(MockitoJUnitRunner.class)
public class ApprovalNotifierTest {
  public static final String TEST_KEY = "testKey";

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private PeriodReferenceDataService periodReferenceDataService;

  @Mock
  private NotificationService notificationService;

  @Mock
  private SupervisingUsersReferenceDataService supervisingUsersReferenceDataService;

  @Mock
  private RightReferenceDataService rightReferenceDataService;

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  private MessageService messageService;

  @InjectMocks
  private ApprovalNotifier approvalNotifier;

  private UserDto approver = mock(UserDto.class);
  private UserDto approver2 = mock(UserDto.class);
  private RightDto right = mock(RightDto.class);
  private UUID supervisoryNodeId = UUID.randomUUID();
  private UUID rightId = UUID.randomUUID();
  private UUID programId = UUID.randomUUID();
  private Requisition requisition = mock(Requisition.class);

  private static final String SUBJECT = "subject";
  private static final String CONTENT = "Dear ${approver}: This email is informing you that the "
      + "${requisitionType} requisition submitted on ${submittedDate} for the Period ${periodName} "
      + "and ${programName} at ${facilityName} is ready for review. Please login to review "
      + "the requisition.${requisitionUrl}Thank you.";

  @Before
  public void setUp() throws Exception {
    mockServices();

    Field field = ApprovalNotifier.class.getDeclaredField("requisitionUri");
    field.setAccessible(true);
    field.set(approvalNotifier, "/requisition/");
  }

  @Test
  public void shouldCallNotificationService() throws Exception {
    when(supervisingUsersReferenceDataService.findAll(supervisoryNodeId, rightId, programId))
        .thenReturn(Collections.singletonList(approver));
    when(right.getId()).thenReturn(rightId);
    mockRequisition();
    mockMessages();

    allowNotifyAndMockUsername(approver, "approver");

    mockChangeDate();

    approvalNotifier.notifyApprovers(requisition);

    verify(notificationService).notify(refEq(approver), eq(SUBJECT), contains("Dear approver: "
        + "This email is informing you that the test requisition"));
  }

  @Test
  public void shouldCallNotificationServiceTwoTimesForTwoApproversWithProperUsernameInContent()
      throws Exception {
    when(supervisingUsersReferenceDataService.findAll(supervisoryNodeId, rightId, programId))
        .thenReturn(Arrays.asList(approver, approver2));
    when(right.getId()).thenReturn(rightId);
    mockRequisition();
    mockMessages();
    allowNotifyAndMockUsername(approver, "approver1");
    allowNotifyAndMockUsername(approver2, "approver2");

    mockChangeDate();

    approvalNotifier.notifyApprovers(requisition);
    ArgumentCaptor<String> argument = ArgumentCaptor.forClass(String.class);

    verify(notificationService, times(2)).notify(refEq(approver), eq(SUBJECT), argument.capture());

    List<String> values = argument.getAllValues();

    assertTrue(values.get(0).contains("approver1"));
    assertTrue(values.get(1).contains("approver2"));
  }

  @Test
  public void shouldNotCallNotificationServiceWhenUserNotActive() throws Exception {
    when(supervisingUsersReferenceDataService.findAll(supervisoryNodeId, rightId, programId))
        .thenReturn(Collections.singletonList(approver));
    when(right.getId()).thenReturn(rightId);
    mockRequisition();
    mockMessages();

    when(approver.allowNotify()).thenReturn(true);
    when(approver.activeAndVerified()).thenCallRealMethod();
    when(approver.isVerified()).thenReturn(true);
    when(approver.isActive()).thenReturn(false);

    mockChangeDate();

    approvalNotifier.notifyApprovers(requisition);

    verify(notificationService, times(0)).notify(refEq(approver), eq(SUBJECT), eq(CONTENT));
  }

  @Test
  public void shouldNotCallNotificationServiceWhenUserNotVerified() throws Exception {
    when(supervisingUsersReferenceDataService.findAll(supervisoryNodeId, rightId, programId))
        .thenReturn(Collections.singletonList(approver));
    when(right.getId()).thenReturn(rightId);
    mockRequisition();
    mockMessages();

    when(approver.allowNotify()).thenReturn(true);
    when(approver.activeAndVerified()).thenCallRealMethod();
    when(approver.isVerified()).thenReturn(false);
    when(approver.isActive()).thenReturn(true);

    mockChangeDate();

    approvalNotifier.notifyApprovers(requisition);

    verify(notificationService, times(0)).notify(refEq(approver), eq(SUBJECT), eq(CONTENT));
  }

  @Test
  public void shouldNotCallNotificationServiceWhenUserNotAllowNotify() throws Exception {
    when(supervisingUsersReferenceDataService.findAll(supervisoryNodeId, rightId, programId))
        .thenReturn(Collections.singletonList(approver));
    when(right.getId()).thenReturn(rightId);
    mockRequisition();
    mockMessages();

    when(approver.allowNotify()).thenReturn(false);
    when(approver.activeAndVerified()).thenReturn(true);

    mockChangeDate();

    approvalNotifier.notifyApprovers(requisition);

    verify(notificationService, times(0)).notify(refEq(approver), eq(SUBJECT), eq(CONTENT));
  }

  private void allowNotifyAndMockUsername(UserDto approver, String username) {
    when(approver.allowNotify()).thenReturn(true);
    when(approver.activeAndVerified()).thenReturn(true);
    when(approver.getEmail()).thenReturn(username + "@some.where");
    when(approver.getUsername()).thenReturn(username);
  }

  private void mockChangeDate() {
    StatusChange submitAuditEntry = mock(StatusChange.class);
    when(requisition.getStatusChanges()).thenReturn(Collections.singletonList(submitAuditEntry));
    when(submitAuditEntry.getStatus()).thenReturn(RequisitionStatus.SUBMITTED);
    when(submitAuditEntry.getCreatedDate()).thenReturn(ZonedDateTime.now());
  }

  private void mockRequisition() {
    when(requisition.getSupervisoryNodeId()).thenReturn(supervisoryNodeId);
    when(requisition.getProgramId()).thenReturn(programId);
  }

  private void mockServices() {
    when(rightReferenceDataService.findRight(RightName.REQUISITION_APPROVE)).thenReturn(right);
    when(periodReferenceDataService.findOne(any())).thenReturn(mock(ProcessingPeriodDto.class));
    when(programReferenceDataService.findOne(any())).thenReturn(mock(ProgramDto.class));
    when(facilityReferenceDataService.findOne(any())).thenReturn(mock(FacilityDto.class));
  }

  private void mockMessages() {
    Message.LocalizedMessage localizedMessage = new Message(TEST_KEY).new LocalizedMessage("test");
    when(messageService.localize(any())).thenReturn(localizedMessage);

    localizedMessage = new Message(TEST_KEY).new LocalizedMessage(SUBJECT);
    when(messageService.localize(new Message(REQUISITION_EMAIL_ACTION_REQUIRED_SUBJECT)))
        .thenReturn(localizedMessage);
    localizedMessage = new Message(TEST_KEY).new LocalizedMessage(CONTENT);
    when(messageService.localize(new Message(REQUISITION_EMAIL_ACTION_REQUIRED_CONTENT)))
        .thenReturn(localizedMessage);
  }
}