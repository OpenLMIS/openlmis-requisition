package org.openlmis.requisition.service;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.settings.service.ConfigurationSettingService;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.refEq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_CONVERT_TO_ORDER_CONTENT;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_CONVERT_TO_ORDER_SUBJECT;

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
  private NotificationService notificationService;

  @InjectMocks
  private RequisitionStatusNotifier requisitionStatusNotifier;

  @Before
  public void setUp() throws RequisitionException {
    mockServices();
  }

  @Test
  public void shouldCallNotificationService() throws Exception {
    Requisition requisition = mock(Requisition.class);
    UserDto user = mock(UserDto.class);

    when(configurationSettingService.getStringValue(REQUISITION_EMAIL_CONVERT_TO_ORDER_SUBJECT))
        .thenReturn(REQUISITION_EMAIL_CONVERT_TO_ORDER_SUBJECT);

    when(configurationSettingService.getStringValue(REQUISITION_EMAIL_CONVERT_TO_ORDER_CONTENT))
        .thenReturn(REQUISITION_EMAIL_CONVERT_TO_ORDER_CONTENT);

    requisitionStatusNotifier.notifyConvertToOrder(user, requisition);

    verify(notificationService).notify(refEq(user),
        eq(REQUISITION_EMAIL_CONVERT_TO_ORDER_SUBJECT),
        eq(REQUISITION_EMAIL_CONVERT_TO_ORDER_CONTENT));
  }

  private void mockServices() {
    when(programReferenceDataService.findOne(any())).thenReturn(
        mock(ProgramDto.class));
    when(periodReferenceDataService.findOne(any())).thenReturn(
        mock(ProcessingPeriodDto.class));
  }
}
