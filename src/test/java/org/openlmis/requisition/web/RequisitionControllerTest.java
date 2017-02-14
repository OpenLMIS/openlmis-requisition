package org.openlmis.requisition.web;

import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyList;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.BindingResultException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.RequisitionStatusProcessor;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.StockAdjustmentReasonReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.validate.DraftRequisitionValidator;
import org.openlmis.requisition.validate.RequisitionValidator;
import org.openlmis.requisition.validate.RequisitionVersionValidator;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.AuthenticationHelper;
import org.openlmis.utils.FacilitySupportsProgramHelper;
import org.springframework.validation.Errors;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnusedPrivateField"})
public class RequisitionControllerTest {
  @Rule
  public final ExpectedException exception = ExpectedException.none();

  @Mock
  private RequisitionRepository requisitionRepository;

  @Mock
  private RequisitionService requisitionService;

  @Mock
  private PeriodService periodService;

  @Mock
  private Requisition initiatedRequsition;

  @Mock
  private Requisition submittedRequsition;

  @Mock
  private Requisition authorizedRequsition;

  @Mock
  private Requisition approvedRequsition;

  @Mock
  private RequisitionTemplate template;

  @Mock
  private RequisitionValidator validator;

  @Mock
  private DraftRequisitionValidator draftValidator;

  @Mock
  private RequisitionTemplateRepository templateRepository;

  @Mock
  private StockAdjustmentReasonReferenceDataService stockAdjustmentReasonReferenceDataService;

  @Mock
  private PermissionService permissionService;

  @Mock
  private RequisitionDtoBuilder requisitionDtoBuilder;

  @Mock
  private FacilitySupportsProgramHelper facilitySupportsProgramHelper;

  @Mock
  private AuthenticationHelper authenticationHelper;

  @Mock
  private OrderableReferenceDataService orderableReferenceDataService;

  @Mock
  private SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

  @Mock
  private ConfigurationSettingService configurationSettingService;

  @Mock
  private RequisitionVersionValidator requisitionVersionValidator;

  @Mock
  private RequisitionStatusProcessor requisitionStatusProcessor;

  @InjectMocks
  private RequisitionController requisitionController;

  private UUID programUuid = UUID.randomUUID();
  private UUID facilityUuid = UUID.randomUUID();
  private UUID uuid1 = UUID.fromString("00000000-0000-0000-0000-000000000001");
  private UUID uuid2 = UUID.fromString("00000000-0000-0000-0000-000000000002");
  private UUID uuid3 = UUID.fromString("00000000-0000-0000-0000-000000000003");
  private UUID uuid4 = UUID.fromString("00000000-0000-0000-0000-000000000004");
  private UUID uuid5 = UUID.fromString("00000000-0000-0000-0000-000000000005");

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    List<ProcessingPeriodDto> processingPeriods = generateProcessingPeriods();
    when(initiatedRequsition.getStatus()).thenReturn(RequisitionStatus.INITIATED);
    when(submittedRequsition.getStatus()).thenReturn(RequisitionStatus.SUBMITTED);
    when(authorizedRequsition.getStatus()).thenReturn(RequisitionStatus.AUTHORIZED);
    when(approvedRequsition.getStatus()).thenReturn(RequisitionStatus.APPROVED);

    when(submittedRequsition.getId()).thenReturn(uuid3);
    when(authorizedRequsition.getId()).thenReturn(uuid4);
    when(authorizedRequsition.isApprovable()).thenReturn(true);

    when(periodService.getPeriods(programUuid, facilityUuid, false))
        .thenReturn(processingPeriods);
    when(periodService.getPeriods(programUuid, facilityUuid, true))
        .thenReturn(Collections.singletonList(processingPeriods.get(0)));

    mockRequisitionRepository();
  }

  @Test
  public void shouldReturnCurrentPeriodForEmergency() throws Exception {
    Collection<ProcessingPeriodDto> periods =
        requisitionController.getProcessingPeriodIds(programUuid, facilityUuid, true);

    verify(periodService).getPeriods(programUuid, facilityUuid, true);
    verifyZeroInteractions(periodService, requisitionRepository);

    assertNotNull(periods);
    assertEquals(1, periods.size());

    List<UUID> periodUuids = periods
        .stream()
        .map(ProcessingPeriodDto::getId)
        .collect(Collectors.toList());

    assertTrue(periodUuids.contains(uuid1));
  }

  @Test
  public void shouldSubmitValidInitiatedRequisition() {
    UserDto submitter = mock(UserDto.class);
    when(submitter.getId()).thenReturn(UUID.randomUUID());

    when(initiatedRequsition.getTemplate()).thenReturn(template);
    when(requisitionRepository.findOne(uuid1)).thenReturn(initiatedRequsition);
    when(authenticationHelper.getCurrentUser()).thenReturn(submitter);

    requisitionController.submitRequisition(uuid1);

    verify(initiatedRequsition).submit(eq(Collections.emptyList()), any(UUID.class));
    // we do not update in this endpoint
    verify(initiatedRequsition, never()).updateFrom(any(Requisition.class), anyList(), anyList());
  }

  @Test
  public void shouldNotSubmitInvalidRequisition() {
    doAnswer(invocation -> {
      Errors errors = (Errors) invocation.getArguments()[1];
      errors.reject("requisitionLineItems",
          "approvedQuantity is only available during the approval step of the requisition process");
      return null;
    }).when(validator).validate(eq(initiatedRequsition), any(Errors.class));
    when(initiatedRequsition.getId()).thenReturn(uuid1);

    assertThatThrownBy(() -> requisitionController.submitRequisition(uuid1))
        .isInstanceOf(BindingResultException.class);

    verifyNoSubmitOrUpdate(initiatedRequsition);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldReturnBadRequestWhenRequisitionIdDiffersFromTheOneInUrl() throws Exception {
    RequisitionDto requisitionDto = mock(RequisitionDto.class);
    when(requisitionDto.getId()).thenReturn(uuid1);
    when(requisitionDto.getTemplate()).thenReturn(null);
    when(requisitionDto.getFacility()).thenReturn(mock(FacilityDto.class));
    when(requisitionDto.getProgram()).thenReturn(mock(ProgramDto.class));
    when(requisitionDto.getProcessingPeriod()).thenReturn(mock(ProcessingPeriodDto.class));
    when(initiatedRequsition.getTemplate()).thenReturn(template);
    when(requisitionRepository.findOne(uuid2)).thenReturn(initiatedRequsition);

    requisitionController.updateRequisition(requisitionDto, uuid2);
  }

  @Test
  public void shouldUpdateRequisition() throws Exception {
    RequisitionDto requisitionDto = mock(RequisitionDto.class);

    when(requisitionDto.getId()).thenReturn(uuid1);
    when(requisitionDto.getFacility()).thenReturn(mock(FacilityDto.class));
    when(requisitionDto.getProgram()).thenReturn(mock(ProgramDto.class));
    when(requisitionDto.getProcessingPeriod()).thenReturn(mock(ProcessingPeriodDto.class));
    when(requisitionDto.getSupervisoryNode()).thenReturn(UUID.randomUUID());

    when(initiatedRequsition.getTemplate()).thenReturn(template);
    when(initiatedRequsition.getSupervisoryNodeId()).thenReturn(null);
    when(initiatedRequsition.getId()).thenReturn(uuid1);

    requisitionController.updateRequisition(requisitionDto, uuid1);

    assertEquals(template, initiatedRequsition.getTemplate());
    verify(initiatedRequsition).updateFrom(any(Requisition.class), anyList(), anyList());
    verify(requisitionRepository).save(initiatedRequsition);
    verify(requisitionVersionValidator).validateRequisitionTimestamps(any(Requisition.class),
        eq(initiatedRequsition));
    verify(stockAdjustmentReasonReferenceDataService)
        .getStockAdjustmentReasonsByProgram(any(UUID.class));
    verifySupervisoryNodeWasNotUpdated(initiatedRequsition);
  }

  @Test
  public void shouldNotUpdateWithInvalidRequisition() {
    RequisitionDto requisitionDto = mock(RequisitionDto.class);
    when(requisitionDto.getTemplate()).thenReturn(template);
    when(requisitionDto.getFacility()).thenReturn(mock(FacilityDto.class));
    when(requisitionDto.getProgram()).thenReturn(mock(ProgramDto.class));
    when(requisitionDto.getProcessingPeriod()).thenReturn(mock(ProcessingPeriodDto.class));

    doAnswer(invocation -> {
      Errors errors = (Errors) invocation.getArguments()[1];
      errors.reject("requisitionLineItems[0].beginningBalance", "Bad argument");

      return null;
    }).when(draftValidator).validate(any(Requisition.class), any(Errors.class));

    assertThatThrownBy(() -> requisitionController.updateRequisition(requisitionDto, uuid1))
        .isInstanceOf(BindingResultException.class);

    verifyNoSubmitOrUpdate(initiatedRequsition);
  }

  @Test
  public void shouldThrowExceptionWhenFacilityOrProgramIdNotFound() throws Exception {
    exception.expect(ValidationMessageException.class);
    requisitionController.initiate(programUuid, null, null, false);
    exception.expect(ValidationMessageException.class);
    requisitionController.initiate(null, facilityUuid, null, false);
  }

  @Test
  public void shouldApproveAuthorizedRequisitionWithParentNode() {
    SupervisoryNodeDto supervisoryNode = mockSupervisoryNode();

    UUID parentNodeId = UUID.randomUUID();
    SupervisoryNodeDto parentNode = mock(SupervisoryNodeDto.class);
    when(parentNode.getId()).thenReturn(parentNodeId);
    when(supervisoryNode.getParentNode()).thenReturn(parentNode);

    requisitionController.approveRequisition(authorizedRequsition.getId());

    verify(authorizedRequsition, times(1)).approve(eq(parentNodeId), any());
  }

  @Test
  public void shouldApproveAuthorizedRequisitionWithoutParentNode() {
    mockSupervisoryNode();

    requisitionController.approveRequisition(authorizedRequsition.getId());

    verify(authorizedRequsition, times(1)).approve(eq(null), any());
  }

  @Test
  public void shouldRejectRequisitionWhenUserCanApproveRequisition() {
    requisitionController.rejectRequisition(authorizedRequsition.getId());

    verify(requisitionService, times(1)).reject(authorizedRequsition.getId());
  }

  @Test
  public void shouldNotRejectRequisitionWhenUserCanNotApproveRequisition() {
    doThrow(PermissionMessageException.class)
        .when(permissionService).canApproveRequisition(uuid4);

    assertThatThrownBy(() -> requisitionController.rejectRequisition(uuid4))
        .isInstanceOf(PermissionMessageException.class);

    verify(requisitionService, times(0)).reject(uuid4);
  }

  @Test
  public void shouldProcessStatusChangeWhenApprovingRequisition() throws Exception {
    requisitionController.approveRequisition(authorizedRequsition.getId());

    verify(requisitionStatusProcessor).statusChange(authorizedRequsition);
  }

  @Test
  public void shouldProcessStatusChangeWhenAuthorizingRequisition() throws Exception {
    UserDto submitter = mock(UserDto.class);
    when(submitter.getId()).thenReturn(UUID.randomUUID());
    when(authenticationHelper.getCurrentUser()).thenReturn(submitter);

    mockSupervisoryNodeForAuthorize();

    requisitionController.authorizeRequisition(submittedRequsition.getId());

    verify(requisitionStatusProcessor).statusChange(submittedRequsition);
  }

  private void mockSupervisoryNodeForAuthorize() {
    SupervisoryNodeDto supervisoryNode = mock(SupervisoryNodeDto.class);
    when(supervisoryNode.getId()).thenReturn(UUID.randomUUID());

    when(supervisoryNodeReferenceDataService.findSupervisoryNode(any(), any()))
        .thenReturn(supervisoryNode);
  }

  private SupervisoryNodeDto mockSupervisoryNode() {
    UUID supervisoryNodeId = UUID.randomUUID();
    SupervisoryNodeDto supervisoryNodeDto = mock(SupervisoryNodeDto.class);
    when(supervisoryNodeDto.getId()).thenReturn(supervisoryNodeId);
    when(supervisoryNodeReferenceDataService.findOne(supervisoryNodeId))
        .thenReturn(supervisoryNodeDto);
    when(authorizedRequsition.getSupervisoryNodeId()).thenReturn(supervisoryNodeId);
    return supervisoryNodeDto;
  }

  private List<ProcessingPeriodDto> generateProcessingPeriods() {
    ProcessingPeriodDto period = new ProcessingPeriodDto();
    period.setId(uuid1);
    ProcessingPeriodDto period2 = new ProcessingPeriodDto();
    period2.setId(uuid2);
    ProcessingPeriodDto period3 = new ProcessingPeriodDto();
    period3.setId(uuid3);
    ProcessingPeriodDto period4 = new ProcessingPeriodDto();
    period4.setId(uuid4);
    ProcessingPeriodDto period5 = new ProcessingPeriodDto();
    period5.setId(uuid5);

    List<ProcessingPeriodDto> periods = new ArrayList<>();
    periods.add(period);
    periods.add(period2);
    periods.add(period3);
    periods.add(period4);
    periods.add(period5);

    return periods;
  }

  private void mockRequisitionRepository() {
    when(requisitionRepository.searchRequisitions(uuid1, facilityUuid, programUuid, false))
        .thenReturn(new ArrayList<>());
    when(requisitionRepository.searchRequisitions(uuid2, facilityUuid, programUuid, false))
        .thenReturn(Collections.singletonList(initiatedRequsition));
    when(requisitionRepository.searchRequisitions(uuid3, facilityUuid, programUuid, false))
        .thenReturn(Collections.singletonList(submittedRequsition));
    when(requisitionRepository.searchRequisitions(uuid4, facilityUuid, programUuid, false))
        .thenReturn(Collections.singletonList(authorizedRequsition));
    when(requisitionRepository.searchRequisitions(uuid5, facilityUuid, programUuid, false))
        .thenReturn(Collections.singletonList(approvedRequsition));
    when(requisitionRepository.save(initiatedRequsition))
        .thenReturn(initiatedRequsition);
    when(requisitionRepository.findOne(uuid1))
        .thenReturn(initiatedRequsition);
    when(requisitionRepository.findOne(authorizedRequsition.getId()))
        .thenReturn(authorizedRequsition);
    when(requisitionRepository.findOne(submittedRequsition.getId()))
        .thenReturn(submittedRequsition);
  }

  private void verifyNoSubmitOrUpdate(Requisition requisition) {
    verifyZeroInteractions(requisitionService);
    verify(requisition, never()).updateFrom(any(Requisition.class), anyList(), anyList());
    verify(requisition, never()).submit(eq(Collections.emptyList()), any(UUID.class));
  }

  private void verifySupervisoryNodeWasNotUpdated(Requisition requisition) {
    verify(requisition, never()).setSupervisoryNodeId(any());
    assertNull(requisition.getSupervisoryNodeId());
  }
}
