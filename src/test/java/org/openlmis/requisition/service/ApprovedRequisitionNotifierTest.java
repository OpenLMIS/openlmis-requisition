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
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_REQUISITION_APPROVED_CONTENT;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_EMAIL_REQUISITION_APPROVED_SUBJECT;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_EMERGENCY;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_REGULAR;

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
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.requisition.web.RequisitionForConvertBuilder;
import org.openlmis.utils.AuthenticationHelper;
import org.openlmis.utils.Message;
import org.openlmis.utils.RightName;

import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.UUID;

@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings("PMD.TooManyMethods")
public class ApprovedRequisitionNotifierTest {

  private static final String SUBJECT = "Action Required";
  private static final String CONTENT = "Dear ${user}:\\n"
      + "This email is informing you that the ${requisitionType} requisition approved on "
      + "${finalApprovalDate} for the Period ${period} and ${program} at ${facility} is ready to "
      + "be converted to an order. Please login to convert the requisition to an order.\\n"
      + "${url}\\n"
      + "Thank you.";

  @Mock
  private UserReferenceDataService userReferenceDataService;

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  private NotificationService notificationService;

  @Mock
  private AuthenticationHelper authenticationHelper;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private PeriodReferenceDataService periodReferenceDataService;

  @Mock
  private MessageService messageService;

  @Mock
  private RequisitionForConvertBuilder requisitionForConvertBuilder;

  @InjectMocks
  private ApprovedRequisitionNotifier approvedRequisitionNotifier;

  private UserDto clerkOne = mock(UserDto.class);
  private UserDto clerkTwo = mock(UserDto.class);
  private UserDto clerkThree = mock(UserDto.class);
  private UserDto clerkFour = mock(UserDto.class);

  private FacilityDto warehouseOne = mock(FacilityDto.class);
  private FacilityDto warehouseTwo = mock(FacilityDto.class);

  private UUID requisitionId = UUID.randomUUID();
  private UUID facilityId = UUID.randomUUID();
  private UUID programId = UUID.randomUUID();
  private UUID processingPeriodId = UUID.randomUUID();
  private UUID warehouseOneId = UUID.randomUUID();
  private UUID warehouseTwoId = UUID.randomUUID();
  private UUID rightId = UUID.randomUUID();

  private Requisition requisition = mock(Requisition.class);
  private FacilityDto facility = mock(FacilityDto.class);
  private ProgramDto program = mock(ProgramDto.class);
  private ProcessingPeriodDto processingPeriod = mock(ProcessingPeriodDto.class);

  private StatusChange statusChange = mock(StatusChange.class);
  private ZonedDateTime createdDate = ZonedDateTime.parse("2017-05-08T10:15:30+01:00");

  private Message regularRequisitionMessage = new Message(REQUISITION_TYPE_REGULAR);
  private Message emergencyRequisitionMessage = new Message(REQUISITION_TYPE_EMERGENCY);

  private RightDto right = mock(RightDto.class);

  @Before
  public void setUp() {
    when(facility.getName()).thenReturn("Mock Facility");
    when(program.getName()).thenReturn("Mock Program");
    when(processingPeriod.getName()).thenReturn("Mock Period");
    when(warehouseOne.getId()).thenReturn(warehouseOneId);
    when(warehouseTwo.getId()).thenReturn(warehouseTwoId);
    when(right.getId()).thenReturn(rightId);

    mockClerk(clerkOne, "ClerkOne");
    mockClerk(clerkTwo, "ClerkTwo");
    mockClerk(clerkThree, "ClerkThree");
    mockClerk(clerkFour, "ClerkFour");
    prepareStatusChange();
    prepareRequisition();
    mockServices();
  }

  @Test
  public void notifyClerksShouldNotifyAllClerksOnce() {
    approvedRequisitionNotifier.notifyClerks(requisition);

    verify(notificationService, times(1))
        .notify(eq(clerkOne), any(), any());
    verify(notificationService, times(1))
        .notify(eq(clerkTwo), any(), any());
    verify(notificationService, times(1))
        .notify(eq(clerkThree), any(), any());
    verify(notificationService, times(1))
        .notify(eq(clerkFour), any(), any());
  }

  @Test
  public void notifyClerkShouldNotifyWithCorrectSubject() {
    approvedRequisitionNotifier.notifyClerks(requisition);

    verify(notificationService, times(4)).notify(any(), eq(SUBJECT), any());
  }

  @Test
  public void notifyClerkShouldNotifyWithCorrectMessageBody() {
    String expectedContent = "Dear ClerkOne:\\n"
        + "This email is informing you that the regular requisition approved on May 8, 2017 "
        + "10:15:30 AM for the Period Mock Period and Mock Program at Mock Facility is ready to be "
        + "converted to an order. Please login to convert the requisition to an order.\\n"
        + System.getenv("BASE_URL") + "/#!/requisitions/convertToOrder\\n"
        + "Thank you.";

    approvedRequisitionNotifier.notifyClerks(requisition);

    verify(notificationService).notify(eq(clerkOne), any(), eq(expectedContent));
  }

  @Test
  public void notifyClerkShouldIgnoreUsersThatCanNotBeNotified() {
    when(clerkOne.allowNotify()).thenReturn(false);

    approvedRequisitionNotifier.notifyClerks(requisition);

    verify(notificationService, never())
        .notify(eq(clerkOne), any(), any());
    verify(notificationService, times(1))
        .notify(eq(clerkTwo), any(), any());
    verify(notificationService, times(1))
        .notify(eq(clerkThree), any(), any());
    verify(notificationService, times(1))
        .notify(eq(clerkFour), any(), any());
  }

  @Test
  public void notifyClerkShouldIgnoreUsersThatAreNotVerified() {
    when(clerkOne.activeAndVerified()).thenCallRealMethod();
    when(clerkOne.isActive()).thenReturn(true);
    when(clerkOne.isVerified()).thenReturn(false);

    approvedRequisitionNotifier.notifyClerks(requisition);

    verify(notificationService, never())
        .notify(eq(clerkOne), any(), any());
    verify(notificationService, times(1))
        .notify(eq(clerkTwo), any(), any());
    verify(notificationService, times(1))
        .notify(eq(clerkThree), any(), any());
    verify(notificationService, times(1))
        .notify(eq(clerkFour), any(), any());
  }

  @Test
  public void notifyClerkShouldIgnoreUsersWithoutEmail() {
    when(clerkOne.getEmail()).thenReturn(null);

    approvedRequisitionNotifier.notifyClerks(requisition);

    verify(notificationService, never())
        .notify(eq(clerkOne), any(), any());
    verify(notificationService, times(1))
        .notify(eq(clerkTwo), any(), any());
    verify(notificationService, times(1))
        .notify(eq(clerkThree), any(), any());
    verify(notificationService, times(1))
        .notify(eq(clerkFour), any(), any());
  }

  private void mockServices() {

    when(facilityReferenceDataService.findOne(eq(facilityId))).thenReturn(facility);
    when(programReferenceDataService.findOne(eq(programId))).thenReturn(program);
    when(periodReferenceDataService.findOne(eq(processingPeriodId))).thenReturn(processingPeriod);
    when(requisitionForConvertBuilder.getAvailableSupplyingDepots(eq(requisitionId)))
        .thenReturn(Arrays.asList(warehouseOne, warehouseTwo));
    when(authenticationHelper.getRight(RightName.ORDERS_EDIT)).thenReturn(right);
    when(userReferenceDataService.findUsers(
        rightId,
        null,
        null,
        warehouseOneId)
    ).thenReturn(Arrays.asList(clerkOne, clerkTwo, clerkThree));
    when(userReferenceDataService.findUsers(
        rightId,
        null,
        null,
        warehouseTwoId
    )).thenReturn(Arrays.asList(clerkThree, clerkTwo, clerkFour));

    mockMessages();
  }

  private void mockMessages() {
    when(messageService.localize(regularRequisitionMessage))
        .thenReturn(regularRequisitionMessage.new LocalizedMessage("regular"));
    when(messageService.localize(emergencyRequisitionMessage))
        .thenReturn(emergencyRequisitionMessage.new LocalizedMessage("emergency"));

    Message requisitionApprovedSubject =
        new Message(REQUISITION_EMAIL_REQUISITION_APPROVED_SUBJECT);
    Message.LocalizedMessage localizedMessage =
        requisitionApprovedSubject.new LocalizedMessage(SUBJECT);
    when(messageService.localize(requisitionApprovedSubject))
        .thenReturn(localizedMessage);
    Message requisitionApprovedContent =
        new Message(REQUISITION_EMAIL_REQUISITION_APPROVED_CONTENT);
    localizedMessage = requisitionApprovedContent.new LocalizedMessage(CONTENT);
    when(messageService.localize(requisitionApprovedContent))
        .thenReturn(localizedMessage);
  }

  private void prepareStatusChange() {
    when(statusChange.getStatus()).thenReturn(RequisitionStatus.APPROVED);
    when(statusChange.getCreatedDate()).thenReturn(createdDate);
  }

  private void prepareRequisition() {
    when(requisition.getId()).thenReturn(requisitionId);
    when(requisition.getFacilityId()).thenReturn(facilityId);
    when(requisition.getProgramId()).thenReturn(programId);
    when(requisition.getProcessingPeriodId()).thenReturn(processingPeriodId);
    when(requisition.getEmergency()).thenReturn(false);
    when(requisition.getStatusChanges()).thenReturn(Arrays.asList(statusChange));
  }

  private void mockClerk(UserDto clerk, String username) {
    when(clerk.getUsername()).thenReturn(username);
    when(clerk.allowNotify()).thenReturn(true);
    when(clerk.getEmail()).thenReturn("someEmail");
    when(clerk.activeAndVerified()).thenReturn(true);
  }

}