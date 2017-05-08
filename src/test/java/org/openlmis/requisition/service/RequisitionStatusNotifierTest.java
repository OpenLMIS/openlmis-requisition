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
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.refEq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_STATUS_UPDATE_CONTENT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_STATUS_UPDATE_SUBJECT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_URI;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusChange;
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

import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.UUID;

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
    when(requisition.getStatus()).thenReturn(RequisitionStatus.AUTHORIZED);

    StatusChange initiateAuditEntry = mock(StatusChange.class);
    StatusChange submitAuditEntry = mock(StatusChange.class);
    StatusChange authorizeAuditEntry = mock(StatusChange.class);
    List<StatusChange> statusChanges = new ArrayList<>();
    statusChanges.add(initiateAuditEntry);
    statusChanges.add(submitAuditEntry);
    statusChanges.add(authorizeAuditEntry);

    when(requisition.getStatusChanges()).thenReturn(statusChanges);
    when(initiateAuditEntry.getStatus()).thenReturn(RequisitionStatus.INITIATED);
    when(initiateAuditEntry.getAuthorId()).thenReturn(userId);
    when(submitAuditEntry.getStatus()).thenReturn(RequisitionStatus.SUBMITTED);
    when(submitAuditEntry.getCreatedDate()).thenReturn(ZonedDateTime.now());
    when(authorizeAuditEntry.getStatus()).thenReturn(RequisitionStatus.AUTHORIZED);
    when(authorizeAuditEntry.getAuthorId()).thenReturn(userId);
    when(authorizeAuditEntry.getCreatedDate()).thenReturn(ZonedDateTime.now());

    when(user.getAllowNotify()).thenReturn(true);
    when(user.isActive()).thenReturn(true);
    when(user.isVerified()).thenReturn(true);
    when(user.getEmail()).thenReturn("some@email.com");

    when(configurationSettingService.getStringValue(REQUISITION_EMAIL_STATUS_UPDATE_SUBJECT))
        .thenReturn(REQUISITION_EMAIL_STATUS_UPDATE_SUBJECT);

    when(configurationSettingService.getStringValue(REQUISITION_EMAIL_STATUS_UPDATE_CONTENT))
        .thenReturn(REQUISITION_EMAIL_STATUS_UPDATE_CONTENT);

    when(configurationSettingService.getStringValue(REQUISITION_URI))
        .thenReturn("/requisition/");

    requisitionStatusNotifier.notifyStatusChanged(requisition);

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
