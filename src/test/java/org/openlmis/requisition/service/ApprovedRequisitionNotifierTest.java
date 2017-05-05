package org.openlmis.requisition.service;

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
import org.openlmis.utils.AuthenticationHelper;
import org.openlmis.utils.Message;

import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.UUID;

import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_EMERGENCY;
import static org.openlmis.requisition.i18n.MessageKeys.REQUISITION_TYPE_REGULAR;

@RunWith(MockitoJUnitRunner.class)
public class ApprovedRequisitionNotifierTest {

  @Mock
  private ConfigurationSettingService configurationSettingService;

  @Mock
  private UserReferenceDataService userReferenceDataService;

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  private NotificationService notificationService;

  @Mock
  private AuthenticationHelper authenticationHelper;

  @Mock
  private RequisitionService requisitionService;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private PeriodReferenceDataService periodReferenceDataService;

  @Mock
  private MessageService messageService;

  @InjectMocks
  private ApprovedRequisitionNotifier approvedRequisitionNotifier;

  private UserDto clerkOne = mock(UserDto.class);
  private UserDto clerkTwo = mock(UserDto.class);
  private UserDto clertThree = mock(UserDto.class);
  private UserDto clertFour = mock(UserDto.class);

  private FacilityDto warehouseOne = mock(FacilityDto.class);
  private FacilityDto warehouseTwo = mock(FacilityDto.class);

  private UUID requisitionId = UUID.randomUUID();
  private UUID facilityId = UUID.randomUUID();
  private UUID programId = UUID.randomUUID();
  private UUID processingPeriodId = UUID.randomUUID();
  private UUID warehouseOneId = UUID.randomUUID();
  private UUID warehouseTwoId = UUID.randomUUID();

  private Requisition requisition = mock(Requisition.class);
  private FacilityDto facility = mock(FacilityDto.class);
  private ProgramDto program = mock(ProgramDto.class);
  private ProcessingPeriodDto processingPeriod = mock(ProcessingPeriodDto.class);

  private StatusChange statusChange = mock(StatusChange.class);
  private ZonedDateTime createdDate = ZonedDateTime.now();

  private Message regularRequisitionMessage = new Message(REQUISITION_TYPE_REGULAR);
  private Message emergencyRequisitionMessage = new Message(REQUISITION_TYPE_EMERGENCY);

  @Before
  public void setUp() {
    when(facility.getName()).thenReturn("Mock Facility");
    when(program.getName()).thenReturn("Mock Program");
    when(processingPeriod.getName()).thenReturn("Mock Period");
    when(warehouseOne.getId()).thenReturn(warehouseOneId);
    

    prepareStatusChange();
    prepareRequisition();
    mockServices();
  }

  @Test
  public void notifyClerksShouldCallNotificationService() {
    approvedRequisitionNotifier.notifyClerks(requisition);
  }

  @Test
  public void notifyClerksShouldNotifyAllClerksOnce() {

  }

  private void mockServices() {
    when(messageService.localize(eq(regularRequisitionMessage)))
        .thenReturn(regularRequisitionMessage.new LocalizedMessage("regular"));
    when(messageService.localize(eq(emergencyRequisitionMessage)))
        .thenReturn(emergencyRequisitionMessage.new LocalizedMessage("emergency"));

    when(facilityReferenceDataService.findOne(eq(facilityId))).thenReturn(facility);
    when(programReferenceDataService.findOne(eq(programId))).thenReturn(program);
    when(periodReferenceDataService.findOne(eq(processingPeriodId))).thenReturn(processingPeriod);
    when(requisitionService.getAvailableSupplyingDepots(eq(requisitionId)))
        .thenReturn(Arrays.asList(warehouseOne, warehouseTwo));
  }

  private void prepareStatusChange() {
    when(statusChange.getStatus()).thenReturn(RequisitionStatus.APPROVED);
    when(statusChange.getCreatedDate()).thenReturn(createdDate);
  }

  private void prepareRequisition() {
    when(requisition.getFacilityId()).thenReturn(facilityId);
    when(requisition.getProgramId()).thenReturn(programId);
    when(requisition.getProcessingPeriodId()).thenReturn(processingPeriodId);
    when(requisition.getEmergency()).thenReturn(false);
    when(requisition.getStatusChanges()).thenReturn(Arrays.asList(statusChange));
  }

}