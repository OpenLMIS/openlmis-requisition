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

package org.openlmis.requisition.web;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.assertj.core.util.Maps.newHashMap;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyCollection;
import static org.mockito.Matchers.anyList;
import static org.mockito.Matchers.anyMap;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import com.google.common.collect.ImmutableList;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.RequisitionValidationService;
import org.openlmis.requisition.dto.BasicRequisitionTemplateDto;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.SupplyLineDto;
import org.openlmis.requisition.dto.SupportedProgramDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.exception.BindingResultException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.exception.VersionMismatchException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.RequisitionStatusNotifier;
import org.openlmis.requisition.service.RequisitionStatusProcessor;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupplyLineReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.StockEventStockManagementService;
import org.openlmis.requisition.settings.service.ConfigurationSettingService;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.testutils.ProgramDtoDataBuilder;
import org.openlmis.requisition.testutils.SupplyLineDtoDataBuilder;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.openlmis.requisition.utils.DateHelper;
import org.openlmis.requisition.utils.DatePhysicalStockCountCompletedEnabledPredicate;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.utils.StockEventBuilder;
import org.openlmis.requisition.validate.RequisitionVersionValidator;

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
  private BasicRequisitionTemplateDto templateDto;

  @Mock
  private RequisitionTemplateRepository templateRepository;

  @Mock
  private PermissionService permissionService;

  @Mock
  private BasicRequisitionDtoBuilder basicRequisitionDtoBuilder;

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

  @Mock
  private RequisitionStatusNotifier requisitionStatusNotifier;

  @Mock
  private DatePhysicalStockCountCompletedEnabledPredicate predicate;

  @Mock
  private StockEventBuilder stockEventBuilderBuilder;

  @Mock
  private StockEventStockManagementService stockEventService;

  @Mock
  private PeriodReferenceDataService periodReferenceDataService;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private SupplyLineReferenceDataService supplyLineReferenceDataService;

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  private DateHelper dateHelper;

  @InjectMocks
  private RequisitionController requisitionController;

  private UUID programUuid = UUID.randomUUID();
  private UUID facilityUuid = UUID.randomUUID();
  private UUID uuid1 = UUID.fromString("00000000-0000-0000-0000-000000000001");
  private UUID uuid2 = UUID.fromString("00000000-0000-0000-0000-000000000002");
  private UUID uuid3 = UUID.fromString("00000000-0000-0000-0000-000000000003");
  private UUID uuid4 = UUID.fromString("00000000-0000-0000-0000-000000000004");
  private UUID uuid5 = UUID.fromString("00000000-0000-0000-0000-000000000005");
  private ProcessingPeriodDto processingPeriod = mock(ProcessingPeriodDto.class);
  private ValidationResult fieldErrors = ValidationResult
      .fieldErrors(newHashMap("someField", new Message("some-key", "someParam")));
  private String bindingResultMessage = "{someField=some-key: someParam}";

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

    when(predicate.exec(any(UUID.class))).thenReturn(true);
    when(predicate.exec(any(ProgramDto.class))).thenReturn(true);

    mockRequisitionRepository();

    when(periodReferenceDataService.findOne(any(UUID.class)))
        .thenReturn(processingPeriod);

    stubValidations(initiatedRequsition, submittedRequsition, authorizedRequsition,
        approvedRequsition);
    when(dateHelper.getCurrentDateWithSystemZone()).thenReturn(LocalDate.now());
    when(predicate.exec(programUuid)).thenReturn(true);
    mockFindSupervisoryNodeByProgramAndFacility();
  }

  private void stubValidations(Requisition... requisitions) {
    for (Requisition requisition : requisitions) {
      when(requisition.getProgramId()).thenReturn(programUuid);
      when(requisition.validateCanChangeStatus(any(LocalDate.class), anyBoolean()))
          .thenReturn(ValidationResult.success());
    }
  }

  @Test
  public void shouldReturnCurrentPeriodForEmergency() {
    when(permissionService.canInitOrAuthorizeRequisition(programUuid, facilityUuid))
        .thenReturn(ValidationResult.success());

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
    mockDependenciesForSubmit();

    requisitionController.submitRequisition(uuid1);

    verify(initiatedRequsition).submit(eq(Collections.emptyMap()), any(UUID.class), eq(false));
    // we do not update in this endpoint
    verify(initiatedRequsition, never())
        .updateFrom(any(Requisition.class), anyMap(), anyBoolean());
    verify(initiatedRequsition)
        .validateCanChangeStatus(dateHelper.getCurrentDateWithSystemZone(),true);
  }

  @Test
  public void shouldDirectlyAuthorizeInitiatedRequisitionIfAuthorizeStepSkipped() {
    mockDependenciesForSubmit();
    ProgramDto programDto = new ProgramDtoDataBuilder().buildWithSkippedAuthorizationStep();
    doReturn(programDto).when(programReferenceDataService).findOne(any(UUID.class));

    requisitionController.submitRequisition(uuid1);

    verify(initiatedRequsition).submit(eq(Collections.emptyMap()), any(UUID.class), eq(true));
    // we do not update in this endpoint
    verify(initiatedRequsition, never())
        .updateFrom(any(Requisition.class), anyMap(), anyBoolean());
  }

  @Test
  public void shouldSubmitEmergencyRequisitionForAnyPeriod() {
    mockDependenciesForSubmit();
    when(initiatedRequsition.getEmergency()).thenReturn(true);
    when(processingPeriod.getEndDate()).thenReturn(LocalDate.now());

    when(dateHelper.isDateAfterNow(processingPeriod.getEndDate())).thenReturn(true);

    requisitionController.submitRequisition(uuid1);

    verify(initiatedRequsition).submit(eq(Collections.emptyMap()), any(UUID.class), eq(false));
    // we do not update in this endpoint
    verify(initiatedRequsition, never())
        .updateFrom(any(Requisition.class), anyMap(), anyBoolean());
    verify(initiatedRequsition)
        .validateCanChangeStatus(dateHelper.getCurrentDateWithSystemZone(),true);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldRejectSubmitRegularRequisitionForWrongPeriod() {
    mockDependenciesForSubmit();
    when(initiatedRequsition.getEmergency()).thenReturn(false);
    when(processingPeriod.getEndDate()).thenReturn(LocalDate.now());

    when(dateHelper.isDateAfterNow(processingPeriod.getEndDate())).thenReturn(true);

    requisitionController.submitRequisition(uuid1);

    verify(initiatedRequsition).submit(eq(Collections.emptyMap()), any(UUID.class), eq(false));
    // we do not update in this endpoint
    verify(initiatedRequsition, never())
        .updateFrom(any(Requisition.class), anyMap(), anyBoolean());
  }

  @Test
  public void shouldNotSubmitInvalidRequisition() {
    when(permissionService.canSubmitRequisition(initiatedRequsition))
        .thenReturn(ValidationResult.success());
    doReturn(fieldErrors)
        .when(initiatedRequsition)
        .validateCanChangeStatus(any(LocalDate.class), anyBoolean());
    when(initiatedRequsition.getId()).thenReturn(uuid1);

    assertThatThrownBy(() -> requisitionController.submitRequisition(uuid1))
        .isInstanceOf(BindingResultException.class)
        .hasMessage(bindingResultMessage);

    verifyNoSubmitOrUpdate(initiatedRequsition);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldReturnBadRequestWhenRequisitionIdDiffersFromTheOneInUrl() throws Exception {
    RequisitionDto requisitionDto = mock(RequisitionDto.class);
    when(requisitionDto.getId()).thenReturn(uuid1);

    requisitionController.updateRequisition(requisitionDto, uuid2);
  }

  @Test
  public void shouldUpdateRequisition() throws Exception {
    RequisitionDto requisitionDto = mock(RequisitionDto.class);

    when(requisitionDto.getId()).thenReturn(uuid1);
    when(requisitionDto.getFacility()).thenReturn(new FacilityDto());
    when(requisitionDto.getProgram()).thenReturn(new ProgramDto());
    when(requisitionDto.getProcessingPeriod()).thenReturn(new ProcessingPeriodDto());
    when(requisitionDto.getSupervisoryNode()).thenReturn(UUID.randomUUID());

    when(initiatedRequsition.getTemplate()).thenReturn(template);
    when(initiatedRequsition.getSupervisoryNodeId()).thenReturn(null);
    when(initiatedRequsition.getId()).thenReturn(uuid1);
    when(initiatedRequsition.validateCanBeUpdated(any(RequisitionValidationService.class)))
        .thenReturn(ValidationResult.success());

    when(requisitionService.validateCanSaveRequisition(initiatedRequsition))
        .thenReturn(ValidationResult.success());
    when(requisitionVersionValidator.validateRequisitionTimestamps(
        any(Requisition.class), any(Requisition.class))).thenReturn(ValidationResult.success());
    when(programReferenceDataService.findOne(any(UUID.class))).thenReturn(
        new ProgramDtoDataBuilder().build());
    when(facilityReferenceDataService.findOne(any(UUID.class))).thenReturn(
        DtoGenerator.of(FacilityDto.class));

    requisitionController.updateRequisition(requisitionDto, uuid1);

    assertEquals(template, initiatedRequsition.getTemplate());
    verify(initiatedRequsition).updateFrom(any(Requisition.class), anyMap(), eq(true));
    verify(requisitionRepository).save(initiatedRequsition);
    verify(requisitionVersionValidator).validateRequisitionTimestamps(any(Requisition.class),
        eq(initiatedRequsition));
    verifySupervisoryNodeWasNotUpdated(initiatedRequsition);
  }

  @Test
  public void shouldNotUpdateWithInvalidRequisition() {
    RequisitionDto requisitionDto = mock(RequisitionDto.class);
    when(requisitionDto.getTemplate()).thenReturn(templateDto);
    when(requisitionDto.getFacility()).thenReturn(new FacilityDto());
    when(requisitionDto.getProgram()).thenReturn(new ProgramDto());
    when(requisitionDto.getProcessingPeriod()).thenReturn(new ProcessingPeriodDto());
    when(requisitionService.validateCanSaveRequisition(initiatedRequsition))
        .thenReturn(ValidationResult.success());
    when(requisitionVersionValidator.validateRequisitionTimestamps(
        any(Requisition.class), any(Requisition.class))).thenReturn(ValidationResult.success());

    doReturn(ValidationResult.conflict("test")).when(requisitionVersionValidator)
        .validateRequisitionTimestamps(any(Requisition.class), any(Requisition.class));

    assertThatThrownBy(() -> requisitionController.updateRequisition(requisitionDto, uuid1))
        .isInstanceOf(VersionMismatchException.class);

    verify(requisitionService).validateCanSaveRequisition(initiatedRequsition);
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
    SupervisoryNodeDto supervisoryNode = mockSupervisoryNodeForApprove();

    UUID parentNodeId = UUID.randomUUID();
    SupervisoryNodeDto parentNode = mock(SupervisoryNodeDto.class);
    when(parentNode.getId()).thenReturn(parentNodeId);
    when(supervisoryNode.getParentNode()).thenReturn(parentNode);
    when(authorizedRequsition.getStatus()).thenReturn(RequisitionStatus.IN_APPROVAL);

    setUpApprover();

    requisitionController.approveRequisition(authorizedRequsition.getId());

    verify(requisitionService, times(1)).validateCanApproveRequisition(
        any(Requisition.class),
        any(UUID.class));

    verify(requisitionService, times(1)).doApprove(eq(parentNodeId), any(),
        any(), eq(authorizedRequsition), eq(emptyList()));

    verifyZeroInteractions(stockEventBuilderBuilder, stockEventService);
    verify(authorizedRequsition)
        .validateCanChangeStatus(dateHelper.getCurrentDateWithSystemZone(),true);
  }

  @Test
  public void shouldApproveAuthorizedRequisitionWithParentNodeAndWithoutSupplyLineForProgram() {
    SupervisoryNodeDto supervisoryNode = mockSupervisoryNodeForApprove();

    UUID parentNodeId = UUID.randomUUID();
    SupervisoryNodeDto parentNode = mock(SupervisoryNodeDto.class);
    when(parentNode.getId()).thenReturn(parentNodeId);
    when(supervisoryNode.getParentNode()).thenReturn(parentNode);

    when(supplyLineReferenceDataService.search(authorizedRequsition.getProgramId(),
        authorizedRequsition.getSupervisoryNodeId())).thenReturn(null);
    when(authorizedRequsition.getStatus()).thenReturn(RequisitionStatus.IN_APPROVAL);

    setUpApprover();

    requisitionController.approveRequisition(authorizedRequsition.getId());

    verify(requisitionService, times(1)).validateCanApproveRequisition(
        any(Requisition.class),
        any(UUID.class));

    verify(requisitionService, times(1)).doApprove(eq(parentNodeId), any(),
        any(), eq(authorizedRequsition), eq(null));

    verifyZeroInteractions(stockEventBuilderBuilder, stockEventService);
    verify(authorizedRequsition)
        .validateCanChangeStatus(dateHelper.getCurrentDateWithSystemZone(),true);
  }

  @Test
  public void shouldApproveAuthorizedRequisitionWithParentNodeAndSupplyLineForProgram() {
    SupervisoryNodeDto supervisoryNode = mockSupervisoryNodeForApprove();

    UUID parentNodeId = UUID.randomUUID();
    SupervisoryNodeDto parentNode = mock(SupervisoryNodeDto.class);
    when(parentNode.getId()).thenReturn(parentNodeId);
    when(supervisoryNode.getParentNode()).thenReturn(parentNode);

    final SupplyLineDto supplyLineDto = prepareForApproveWithSupplyLine();

    requisitionController.approveRequisition(authorizedRequsition.getId());

    verify(requisitionService, times(1)).validateCanApproveRequisition(
        any(Requisition.class),
        any(UUID.class));

    verify(requisitionService, times(1)).doApprove(eq(parentNodeId), any(),
        any(), eq(authorizedRequsition), eq(singletonList(supplyLineDto)));
    verify(authorizedRequsition)
        .validateCanChangeStatus(dateHelper.getCurrentDateWithSystemZone(),true);
  }

  @Test
  public void shouldApproveAuthorizedRequisitionWithoutParentNode() {
    final SupplyLineDto supplyLineDto = prepareForApproveWithSupplyLine();
    when(authorizedRequsition.getEmergency()).thenReturn(false);
    StockEventDto stockEventDto = DtoGenerator.of(StockEventDto.class);
    when(stockEventBuilderBuilder.fromRequisition(any(Requisition.class)))
        .thenReturn(stockEventDto);

    requisitionController.approveRequisition(authorizedRequsition.getId());

    verify(requisitionService, times(1)).validateCanApproveRequisition(
        any(Requisition.class),
        any(UUID.class));

    verify(stockEventBuilderBuilder).fromRequisition(authorizedRequsition);
    verify(stockEventService).submit(stockEventDto);
    verify(requisitionService, times(1)).doApprove(eq(null), any(),
        any(), eq(authorizedRequsition), eq(singletonList(supplyLineDto)));
    verify(authorizedRequsition)
        .validateCanChangeStatus(dateHelper.getCurrentDateWithSystemZone(),true);
  }

  @Test
  public void shouldNotSendStockEventOnFinalApprovalForEmergencyRequisition() {
    final SupplyLineDto supplyLineDto = prepareForApproveWithSupplyLine();
    when(authorizedRequsition.getEmergency()).thenReturn(true);

    requisitionController.approveRequisition(authorizedRequsition.getId());

    verify(requisitionService, times(1)).validateCanApproveRequisition(
        any(Requisition.class),
        any(UUID.class));

    verifyZeroInteractions(stockEventBuilderBuilder, stockEventService);
    verify(requisitionService, times(1)).doApprove(eq(null), any(),
        any(), eq(authorizedRequsition), eq(singletonList(supplyLineDto)));
    verify(authorizedRequsition)
        .validateCanChangeStatus(dateHelper.getCurrentDateWithSystemZone(),true);
  }

  @Test
  public void shouldNotApproveInvalidRequisition() {
    setUpApprover();
    doReturn(fieldErrors)
        .when(authorizedRequsition)
        .validateCanChangeStatus(any(LocalDate.class), anyBoolean());

    assertThatThrownBy(() ->
        requisitionController.approveRequisition(authorizedRequsition.getId()))
        .isInstanceOf(BindingResultException.class)
        .hasMessage(bindingResultMessage);

    verifyNoApproveOrUpdate(authorizedRequsition);
  }

  @Test
  public void shouldAutomaticallyConvertIfLocalFulfillsIsSet() {
    when(authorizedRequsition.getStatus()).thenReturn(RequisitionStatus.APPROVED);

    SupplyLineDto supplyLineDto = prepareSupplyLine(authorizedRequsition, true, true);
    setUpApprover();

    requisitionController.approveRequisition(authorizedRequsition.getId());

    ConvertToOrderDto entry = new ConvertToOrderDto(uuid4, supplyLineDto.getSupplyingFacility());
    ImmutableList<ConvertToOrderDto> list = ImmutableList.of(entry);
    verify(requisitionService).convertToOrder(eq(list), any(UserDto.class));
  }

  @Test
  public void shouldNotAutomaticallyConvertIfSupplyingFacilityDoesNotSupportProgram() {
    when(authorizedRequsition.getStatus()).thenReturn(RequisitionStatus.APPROVED);

    prepareSupplyLine(authorizedRequsition, true, false);
    setUpApprover();

    requisitionController.approveRequisition(authorizedRequsition.getId());

    verify(requisitionService, never()).convertToOrder(anyList(), any(UserDto.class));
  }

  @Test(expected = PermissionMessageException.class)
  public void shouldNotApproveIfHasNoPermission() {
    mockSupervisoryNodeForApprove();

    when(authenticationHelper.getCurrentUser()).thenReturn(DtoGenerator.of(UserDto.class));

    PermissionMessageException exception = mock(PermissionMessageException.class);
    doThrow(exception).when(requisitionService).validateCanApproveRequisition(
        any(Requisition.class),
        any(UUID.class));

    requisitionController.approveRequisition(authorizedRequsition.getId());
  }

  @Test
  public void shouldRejectRequisitionWhenUserCanApproveRequisition() {
    when(permissionService.canApproveRequisition(authorizedRequsition))
        .thenReturn(ValidationResult.success());
    when(requisitionService.reject(authorizedRequsition, Collections.emptyMap()))
        .thenReturn(initiatedRequsition);

    requisitionController.rejectRequisition(authorizedRequsition.getId());

    verify(requisitionService, times(1)).reject(authorizedRequsition, Collections.emptyMap());
  }

  @Test
  public void shouldNotRejectRequisitionWhenUserCanNotApproveRequisition() {
    doReturn(ValidationResult.noPermission("notAuthorized"))
        .when(permissionService).canApproveRequisition(authorizedRequsition);

    assertThatThrownBy(() -> requisitionController.rejectRequisition(authorizedRequsition.getId()))
        .isInstanceOf(PermissionMessageException.class);

    verify(requisitionService, times(0)).reject(authorizedRequsition, Collections.emptyMap());
  }

  @Test
  public void shouldCallRequisitionStatusNotifierWhenReject() {
    when(permissionService.canApproveRequisition(authorizedRequsition))
        .thenReturn(ValidationResult.success());
    when(requisitionService.reject(authorizedRequsition, Collections.emptyMap()))
        .thenReturn(initiatedRequsition);

    requisitionController.rejectRequisition(authorizedRequsition.getId());

    verify(requisitionStatusNotifier).notifyStatusChanged(initiatedRequsition);
  }

  @Test
  public void shouldProcessStatusChangeWhenApprovingRequisition() throws Exception {
    when(requisitionService.validateCanApproveRequisition(any(Requisition.class),
        any(UUID.class)))
        .thenReturn(ValidationResult.success());
    when(authenticationHelper.getCurrentUser()).thenReturn(DtoGenerator.of(UserDto.class));

    requisitionController.approveRequisition(authorizedRequsition.getId());

    verify(requisitionService, times(1)).validateCanApproveRequisition(
        any(Requisition.class),
        any(UUID.class));

    verify(requisitionStatusProcessor).statusChange(authorizedRequsition);
  }

  @Test
  public void shouldProcessStatusChangeWhenAuthorizingRequisition() {
    setUpAuthorizer();

    requisitionController.authorizeRequisition(submittedRequsition.getId());

    verify(requisitionStatusProcessor).statusChange(submittedRequsition);
  }

  @Test
  public void shouldCallValidationsWhenAuthorizingRequisition() {
    setUpAuthorizer();

    requisitionController.authorizeRequisition(submittedRequsition.getId());

    verify(submittedRequsition)
        .validateCanChangeStatus(dateHelper.getCurrentDateWithSystemZone(),true);
  }

  @Test
  public void shouldNotAuthorizeInvalidRequisition() {
    setUpAuthorizer();
    doReturn(fieldErrors)
        .when(submittedRequsition)
        .validateCanChangeStatus(any(LocalDate.class), anyBoolean());

    assertThatThrownBy(() ->
        requisitionController.authorizeRequisition(submittedRequsition.getId()))
        .isInstanceOf(BindingResultException.class)
        .hasMessage(bindingResultMessage);

    verifyNoAuthorizeOrUpdate(submittedRequsition);
  }

  private void mockDependenciesForSubmit() {
    when(permissionService.canSubmitRequisition(initiatedRequsition))
        .thenReturn(ValidationResult.success());

    when(initiatedRequsition.getTemplate()).thenReturn(template);
    when(requisitionRepository.findOne(uuid1)).thenReturn(initiatedRequsition);
    when(programReferenceDataService.findOne(any(UUID.class))).thenReturn(
        new ProgramDtoDataBuilder().buildWithNotSkippedAuthorizationStep());
    when(authenticationHelper.getCurrentUser()).thenReturn(DtoGenerator.of(UserDto.class));
  }

  private void mockFindSupervisoryNodeByProgramAndFacility() {
    SupervisoryNodeDto supervisoryNode = mock(SupervisoryNodeDto.class);
    when(supervisoryNode.getId()).thenReturn(UUID.randomUUID());

    when(supervisoryNodeReferenceDataService.findSupervisoryNode(any(), any()))
        .thenReturn(supervisoryNode);
  }

  private SupervisoryNodeDto mockSupervisoryNodeForApprove() {
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
    verifyNoMoreInteractions(requisitionService);
    verify(requisition, never()).updateFrom(any(Requisition.class), anyMap(),
        anyBoolean());
    verify(requisition, never()).validateCanBeUpdated(any(RequisitionValidationService.class));
    verify(requisition, never()).submit(eq(emptyMap()), any(UUID.class), anyBoolean());
  }

  private void verifyNoApproveOrUpdate(Requisition requisition) {
    verify(requisition, never()).updateFrom(any(Requisition.class), anyMap(),
        anyBoolean());
    verify(requisition, never()).validateCanBeUpdated(any(RequisitionValidationService.class));
    verify(requisition, never())
        .approve(any(UUID.class), anyMap(), anyCollection(), any(UUID.class));
  }

  private void verifyNoAuthorizeOrUpdate(Requisition requisition) {
    verify(requisition, never()).updateFrom(any(Requisition.class), anyMap(),
        anyBoolean());
    verify(requisition, never()).validateCanBeUpdated(any(RequisitionValidationService.class));
    verify(requisition, never())
        .authorize(anyMap(), any(UUID.class));
  }

  private void verifySupervisoryNodeWasNotUpdated(Requisition requisition) {
    verify(requisition, never()).setSupervisoryNodeId(any());
    assertNull(requisition.getSupervisoryNodeId());
  }

  private void setUpApprover() {
    when(authenticationHelper.getCurrentUser()).thenReturn(DtoGenerator.of(UserDto.class));
    when(requisitionService.validateCanApproveRequisition(any(Requisition.class),
        any(UUID.class)))
        .thenReturn(ValidationResult.success());
  }

  private void setUpAuthorizer() {
    when(permissionService.canAuthorizeRequisition(submittedRequsition))
        .thenReturn(ValidationResult.success());
    when(authenticationHelper.getCurrentUser()).thenReturn(DtoGenerator.of(UserDto.class));
  }

  private SupplyLineDto prepareSupplyLine(Requisition requisition, boolean locallyFulfills,
                                          boolean withSupportedProgram) {
    SupplyLineDto supplyLine = new SupplyLineDtoDataBuilder().build();

    FacilityDto facility = new FacilityDto();
    facility.setId(supplyLine.getSupplyingFacility());

    SupportedProgramDto supportedProgram = new SupportedProgramDto();
    supportedProgram.setSupportLocallyFulfilled(locallyFulfills);

    when(supplyLineReferenceDataService.search(
        requisition.getProgramId(), requisition.getSupervisoryNodeId()))
        .thenReturn(Collections.singletonList(supplyLine));
    when(facilityReferenceDataService.findOne(supplyLine.getSupplyingFacility()))
        .thenReturn(facility);
    when(facilitySupportsProgramHelper.getSupportedProgram(facility, requisition.getProgramId()))
        .thenReturn(withSupportedProgram ? supportedProgram : null);

    return supplyLine;
  }

  private SupplyLineDto prepareForApproveWithSupplyLine() {
    when(authorizedRequsition.getStatus()).thenReturn(RequisitionStatus.APPROVED);
    final SupplyLineDto supplyLineDto = prepareSupplyLine(authorizedRequsition, false, true);
    setUpApprover();
    return supplyLineDto;
  }

}
