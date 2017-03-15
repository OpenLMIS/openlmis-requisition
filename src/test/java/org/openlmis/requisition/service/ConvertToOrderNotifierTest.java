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
import static org.mockito.Matchers.refEq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_CONVERT_TO_ORDER_CONTENT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_CONVERT_TO_ORDER_SUBJECT;

import java.util.Collections;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusChange;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.settings.service.ConfigurationSettingService;

@SuppressWarnings({"PMD.UnusedPrivateField"})
@RunWith(MockitoJUnitRunner.class)
public class ConvertToOrderNotifierTest {

  @Mock
  private ConfigurationSettingService configurationSettingService;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private PeriodReferenceDataService periodReferenceDataService;

  @Mock
  private NotificationService notificationService;

  @Mock
  private UserReferenceDataService userReferenceDataService;

  @InjectMocks
  private ConvertToOrderNotifier convertToOrderNotifier;

  private UserDto user = mock(UserDto.class);
  private UUID userId = UUID.randomUUID();

  @Before
  public void setUp() {
    mockServices();
  }

  @Test
  public void shouldCallNotificationService() throws Exception {
    Requisition requisition = mock(Requisition.class);
    StatusChange initiateAuditEntry = mock(StatusChange.class);

    when(requisition.getStatusChanges()).thenReturn(Collections.singletonList(initiateAuditEntry));
    when(initiateAuditEntry.getStatus()).thenReturn(RequisitionStatus.INITIATED);
    when(initiateAuditEntry.getAuthorId()).thenReturn(userId);
    when(configurationSettingService.getStringValue(REQUISITION_EMAIL_CONVERT_TO_ORDER_SUBJECT))
        .thenReturn(REQUISITION_EMAIL_CONVERT_TO_ORDER_SUBJECT);

    when(configurationSettingService.getStringValue(REQUISITION_EMAIL_CONVERT_TO_ORDER_CONTENT))
        .thenReturn(REQUISITION_EMAIL_CONVERT_TO_ORDER_CONTENT);

    convertToOrderNotifier.notifyConvertToOrder(requisition);

    verify(notificationService).notify(refEq(user),
        eq(REQUISITION_EMAIL_CONVERT_TO_ORDER_SUBJECT),
        eq(REQUISITION_EMAIL_CONVERT_TO_ORDER_CONTENT));
  }

  private void mockServices() {
    when(programReferenceDataService.findOne(any())).thenReturn(
        mock(ProgramDto.class));
    when(periodReferenceDataService.findOne(any())).thenReturn(
        mock(ProcessingPeriodDto.class));
    when(userReferenceDataService.findOne(eq(userId))).thenReturn(user);
  }
}
