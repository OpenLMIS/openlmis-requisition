package org.openlmis.requisition.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.refEq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_ACTION_REQUIRED_CONTENT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_ACTION_REQUIRED_SUBJECT;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.AuditLogEntry;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
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
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.Message;
import org.openlmis.utils.RightName;

import java.time.ZonedDateTime;
import java.util.Collections;
import java.util.UUID;

@RunWith(MockitoJUnitRunner.class)
public class ApprovalNotifierTest {
  @Mock
  private ConfigurationSettingService configurationSettingService;

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
  private RightDto right = mock(RightDto.class);
  private UUID supervisoryNodeId = UUID.randomUUID();
  private UUID rightId = UUID.randomUUID();
  private UUID programId = UUID.randomUUID();
  private Requisition requisition = mock(Requisition.class);

  private static final String SUBJECT = "subject";
  private static final String CONTENT = "content";

  @Before
  public void setUp() {
    mockServices();
  }

  @Test
  public void shouldCallNotificationService() throws Exception {
    when(right.getId()).thenReturn(rightId);
    mockRequisition();
    mockMessages();

    when(approver.getAllowNotify()).thenReturn(true);
    when(approver.isVerified()).thenReturn(true);
    when(approver.isActive()).thenReturn(true);

    mockChangeDate();

    approvalNotifier.notifyApprovers(requisition);

    verify(notificationService).notify(refEq(approver), eq(SUBJECT), eq(CONTENT));
  }

  @Test
  public void shouldNotCallNotificationServiceWhenUserNotActive() throws Exception {
    when(right.getId()).thenReturn(rightId);
    mockRequisition();
    mockMessages();

    when(approver.getAllowNotify()).thenReturn(true);
    when(approver.isVerified()).thenReturn(true);
    when(approver.isActive()).thenReturn(false);

    mockChangeDate();

    approvalNotifier.notifyApprovers(requisition);

    verify(notificationService, times(0)).notify(refEq(approver), eq(SUBJECT), eq(CONTENT));
  }

  @Test
  public void shouldNotCallNotificationServiceWhenUserNotVerified() throws Exception {
    when(right.getId()).thenReturn(rightId);
    mockRequisition();
    mockMessages();

    when(approver.getAllowNotify()).thenReturn(true);
    when(approver.isVerified()).thenReturn(false);
    when(approver.isActive()).thenReturn(true);

    mockChangeDate();

    approvalNotifier.notifyApprovers(requisition);

    verify(notificationService, times(0)).notify(refEq(approver), eq(SUBJECT), eq(CONTENT));
  }

  @Test
  public void shouldNotCallNotificationServiceWhenUserNotAllowNotify() throws Exception {
    when(right.getId()).thenReturn(rightId);
    mockRequisition();
    mockMessages();

    when(approver.getAllowNotify()).thenReturn(false);
    when(approver.isVerified()).thenReturn(true);
    when(approver.isActive()).thenReturn(true);

    mockChangeDate();

    approvalNotifier.notifyApprovers(requisition);

    verify(notificationService, times(0)).notify(refEq(approver), eq(SUBJECT), eq(CONTENT));
  }

  private void mockChangeDate() {
    AuditLogEntry submitAuditEntry = mock(AuditLogEntry.class);
    when(requisition.getStatusChanges()).thenReturn(Collections.singletonMap(
        RequisitionStatus.SUBMITTED.toString(), submitAuditEntry));
    when(submitAuditEntry.getChangeDate()).thenReturn(ZonedDateTime.now());
  }

  private void mockRequisition() {
    when(requisition.getSupervisoryNodeId()).thenReturn(supervisoryNodeId);
    when(requisition.getProgramId()).thenReturn(programId);
  }

  private void mockServices() {
    when(rightReferenceDataService.findRight(RightName.REQUISITION_APPROVE)).thenReturn(right);
    when(supervisingUsersReferenceDataService.findAll(supervisoryNodeId, rightId, programId))
        .thenReturn(Collections.singletonList(approver));
    when(periodReferenceDataService.findOne(any())).thenReturn(mock(ProcessingPeriodDto.class));
    when(programReferenceDataService.findOne(any())).thenReturn(mock(ProgramDto.class));
    when(facilityReferenceDataService.findOne(any())).thenReturn(mock(FacilityDto.class));
    when(configurationSettingService.getStringValue(REQUISITION_EMAIL_ACTION_REQUIRED_SUBJECT))
        .thenReturn(SUBJECT);
    when(configurationSettingService.getStringValue(REQUISITION_EMAIL_ACTION_REQUIRED_CONTENT))
        .thenReturn(CONTENT);
  }

  private void mockMessages() {
    Message.LocalizedMessage localizedMessage = new Message("test").new LocalizedMessage("test");
    when(messageService.localize(any())).thenReturn(localizedMessage);
  }
}