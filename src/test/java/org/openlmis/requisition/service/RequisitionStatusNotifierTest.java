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
import org.springframework.context.MessageSource;

import java.util.Locale;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.contains;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.refEq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SuppressWarnings({"PMD.UnusedPrivateField"})
@RunWith(MockitoJUnitRunner.class)
public class RequisitionStatusNotifierTest {

  @Mock
  private MessageSource messageSource;

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
    String mailSubject = "requisition.mail.convert-to-order.subject";
    String mailContent = "requisition.mail.convert-to-order.content";

    when(messageSource.getMessage(contains(mailSubject), any(Object[].class),
        any(Locale.class))).thenReturn(mailSubject);

    when(messageSource.getMessage(contains(mailContent), any(Object[].class),
        any(Locale.class))).thenReturn(mailContent);

    requisitionStatusNotifier.notifyConvertToOrder(user, requisition);

    verify(notificationService).notify(refEq(user), eq(mailSubject), eq(mailContent));
  }

  private void mockServices() {
    when(programReferenceDataService.findOne(any())).thenReturn(
        mock(ProgramDto.class));
    when(periodReferenceDataService.findOne(any())).thenReturn(
        mock(ProcessingPeriodDto.class));
  }
}
