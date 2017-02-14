package org.openlmis.requisition.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.refEq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_STATUS_UPDATE_CONTENT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_STATUS_UPDATE_SUBJECT;

import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import org.javers.common.collections.Optional;
import org.javers.core.commit.CommitMetadata;
import org.javers.core.diff.Change;
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
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.Message;
import org.springframework.context.MessageSource;

@SuppressWarnings({"PMD.UnusedPrivateField"})
@RunWith(MockitoJUnitRunner.class)
public class RequisitionStatusNotifierTest {

  @Mock
  private ConfigurationSettingService configurationSettingService;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private PeriodReferenceDataService periodReferenceDataService;

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  private NotificationService notificationService;

  @Mock
  private UserReferenceDataService userReferenceDataService;

  @Mock
  private MessageService messageService;

  @Mock
  private MessageSource messageSource;

  @InjectMocks
  private RequisitionStatusNotifier requisitionStatusNotifier;

  private UserDto user = mock(UserDto.class);
  private UUID userId = UUID.randomUUID();

  @Before
  public void setUp() {
    mockServices();
  }

  @Test
  public void shouldCallNotificationService() throws Exception {
    Requisition requisition = mock(Requisition.class);
    AuditLogEntry initiateAuditEntry = mock(AuditLogEntry.class);
    AuditLogEntry submitAuditEntry = mock(AuditLogEntry.class);
    Map<String, AuditLogEntry> statusChangesMap = new HashMap<>();
    statusChangesMap.put(RequisitionStatus.INITIATED.toString(), initiateAuditEntry);
    statusChangesMap.put(RequisitionStatus.SUBMITTED.toString(), submitAuditEntry);

    when(requisition.getStatusChanges()).thenReturn(statusChangesMap);
    when(initiateAuditEntry.getAuthorId()).thenReturn(userId);
    when(submitAuditEntry.getChangeDate()).thenReturn(ZonedDateTime.now());
    when(requisition.getStatus()).thenReturn(RequisitionStatus.AUTHORIZED);

    Change change = mock(Change.class);
    CommitMetadata commitMetadata = mock(CommitMetadata.class);
    when(change.getCommitMetadata()).thenReturn(Optional.of(commitMetadata));

    when(configurationSettingService.getStringValue(REQUISITION_EMAIL_STATUS_UPDATE_SUBJECT))
        .thenReturn(REQUISITION_EMAIL_STATUS_UPDATE_SUBJECT);

    when(configurationSettingService.getStringValue(REQUISITION_EMAIL_STATUS_UPDATE_CONTENT))
        .thenReturn(REQUISITION_EMAIL_STATUS_UPDATE_CONTENT);

    requisitionStatusNotifier.notifyStatusChanged(requisition, change);

    verify(notificationService).notify(refEq(user),
        eq(REQUISITION_EMAIL_STATUS_UPDATE_SUBJECT),
        eq(REQUISITION_EMAIL_STATUS_UPDATE_CONTENT));
  }

  private void mockServices() {
    when(programReferenceDataService.findOne(any())).thenReturn(
        mock(ProgramDto.class));
    when(periodReferenceDataService.findOne(any())).thenReturn(
        mock(ProcessingPeriodDto.class));
    when(facilityReferenceDataService.findOne(any())).thenReturn(
        mock(FacilityDto.class));
    when(userReferenceDataService.findOne(eq(userId))).thenReturn(user);
    when(messageSource.getMessage(anyString(), any(), any())).thenReturn("test");
    Message.LocalizedMessage localizedMessage = new Message("test")
        .localMessage(messageSource, Locale.getDefault());
    when(messageService.localize(any())).thenReturn(localizedMessage);
  }
}
