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

import static com.google.common.collect.Sets.newHashSet;
import static java.util.Collections.emptyMap;
import static java.util.Collections.singleton;
import static java.util.Collections.singletonList;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anySetOf;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.APPROVED_QUANTITY;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.APPROVED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.AUTHORIZED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.INITIATED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.IN_APPROVAL;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.REJECTED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.RELEASED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.RELEASED_WITHOUT_ORDER;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.SKIPPED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.SUBMITTED;
import static org.openlmis.requisition.utils.Pagination.DEFAULT_PAGE_NUMBER;
import static org.openlmis.requisition.utils.Pagination.NO_PAGINATION;
import static org.openlmis.requisition.utils.Pagination.getPage;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.runners.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;
import org.mockito.stubbing.OngoingStubbing;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.StatusChange;
import org.openlmis.requisition.domain.requisition.StatusMessage;
import org.openlmis.requisition.domain.requisition.StockAdjustmentReason;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.BasicRequisitionDto;
import org.openlmis.requisition.dto.DetailedRoleAssignmentDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.dto.OrderLineItemDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ReasonCategory;
import org.openlmis.requisition.dto.ReasonType;
import org.openlmis.requisition.dto.ReleasableRequisitionDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.RoleDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.SupplyLineDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.dto.stockmanagement.StockCardRangeSummaryDto;
import org.openlmis.requisition.dto.stockmanagement.StockCardSummaryDto;
import org.openlmis.requisition.errorhandling.FailureType;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.openlmis.requisition.service.fulfillment.OrderFulfillmentService;
import org.openlmis.requisition.service.referencedata.ApproveProductsAggregator;
import org.openlmis.requisition.service.referencedata.ApprovedProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.IdealStockAmountReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.RightReferenceDataService;
import org.openlmis.requisition.service.referencedata.ScheduleReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserRoleAssignmentsReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.StockCardRangeSummaryStockManagementService;
import org.openlmis.requisition.service.stockmanagement.StockCardSummariesStockManagementService;
import org.openlmis.requisition.service.stockmanagement.StockOnHandRetrieverBuilderFactory;
import org.openlmis.requisition.settings.service.ConfigurationSettingService;
import org.openlmis.requisition.testutils.ApprovedProductDtoDataBuilder;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.testutils.IdealStockAmountDtoDataBuilder;
import org.openlmis.requisition.testutils.OrderableDtoDataBuilder;
import org.openlmis.requisition.testutils.StockCardRangeSummaryDtoDataBuilder;
import org.openlmis.requisition.testutils.SupplyLineDtoDataBuilder;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.openlmis.requisition.web.BasicRequisitionDtoBuilder;
import org.openlmis.requisition.web.OrderDtoBuilder;
import org.openlmis.requisition.web.RequisitionForConvertBuilder;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.util.ReflectionTestUtils;

@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnusedPrivateField"})
@RunWith(MockitoJUnitRunner.class)
public class RequisitionServiceTest {
  private static final String COMMODITY_TYPE = "commodityType";
  private static final UUID COMMODITY_TYPE_ID = UUID.randomUUID();

  private static final UUID PERIOD_ID = UUID.randomUUID();
  private Requisition requisition;
  private RequisitionDto requisitionDto;

  @Mock
  private Requisition requisitionMock;

  @Mock
  private StatusChange statusChange;

  @Mock
  private RequisitionLineItem lineItem1;

  @Mock
  private RequisitionLineItem lineItem2;

  @Mock
  private ConfigurationSettingService configurationSettingService;

  @Mock
  private RequisitionRepository requisitionRepository;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  private PeriodService periodService;

  @Mock
  private ScheduleReferenceDataService scheduleReferenceDataService;

  @Mock
  private OrderableReferenceDataService orderableReferenceDataService;

  @Mock
  private ApprovedProductReferenceDataService approvedProductReferenceDataService;

  @Mock
  private UserFulfillmentFacilitiesReferenceDataService fulfillmentFacilitiesReferenceDataService;

  @Mock
  private RightReferenceDataService rightReferenceDataService;

  @Mock
  private OrderFulfillmentService orderFulfillmentService;

  @Mock
  private StatusMessageRepository statusMessageRepository;

  @Mock
  private OrderDtoBuilder orderDtoBuilder;

  @Mock
  private UserRoleAssignmentsReferenceDataService userRoleAssignmentsReferenceDataService;

  @Mock
  private PermissionService permissionService;

  @Mock
  private AuthenticationHelper authenticationHelper;

  @Mock
  private RequisitionStatusProcessor requisitionStatusProcessor;

  @Mock
  private RequisitionForConvertBuilder requisitionForConvertBuilder;

  @Mock
  private IdealStockAmountReferenceDataService idealStockAmountReferenceDataService;

  @Mock
  private StockCardSummariesStockManagementService stockCardSummariesStockManagementService;

  @Spy
  private StockOnHandRetrieverBuilderFactory stockOnHandRetrieverBuilderFactory;

  @Mock
  private ProofOfDeliveryService proofOfDeliveryService;

  @Mock
  private StockCardRangeSummaryStockManagementService stockCardRangeSummaryStockManagementService;

  @InjectMocks
  private RequisitionService requisitionService;

  @Mock
  private BasicRequisitionDtoBuilder basicRequisitionDtoBuilder;

  @Spy
  private RequisitionTemplate requisitionTemplate = new RequisitionTemplateDataBuilder()
      .withAllColumns().build();

  private RightDto convertToOrderRight = DtoGenerator.of(RightDto.class, 2).get(0);
  private RightDto approveRequisitionRight = DtoGenerator.of(RightDto.class, 2).get(1);
  private RoleDto role = DtoGenerator.of(RoleDto.class);
  private UserDto user = DtoGenerator.of(UserDto.class);
  private StockCardSummaryDto stockCardSummaryDto = DtoGenerator.of(StockCardSummaryDto.class);
  private ProgramDto program = DtoGenerator.of(ProgramDto.class, true);
  private FacilityDto facility = DtoGenerator.of(FacilityDto.class);
  private SupervisoryNodeDto supervisoryNode = DtoGenerator.of(SupervisoryNodeDto.class);
  private ProcessingPeriodDto processingPeriod = DtoGenerator.of(ProcessingPeriodDto.class);
  private List<String> permissionStrings = singletonList("validPermissionString");
  private PageRequest pageRequest = new PageRequest(DEFAULT_PAGE_NUMBER, NO_PAGINATION);
  private List<StockAdjustmentReason> stockAdjustmentReasons;
  private Requisition previousRequisition;

  private static final int SETTING = 5;
  private static final int ADJUSTED_CONSUMPTION = 7;
  private static final UUID PRODUCT_ID = UUID.randomUUID();
  private static final UUID NON_FULL_PRODUCT_ID = UUID.randomUUID();
  private String productNamePrefix = "Product ";
  private StockCardRangeSummaryDto stockCardRangeSummaryDto;

  @Before
  public void setUp() {
    generateRequisition();
    generateRequisitionDto();
    generateReasons();
    mockRepositories();
    when(permissionService.getPermissionStrings()).thenReturn(permissionStrings);
    ReflectionTestUtils.setField(
        stockOnHandRetrieverBuilderFactory,
        "stockCardSummariesStockManagementService",
        stockCardSummariesStockManagementService
    );

    stockCardRangeSummaryDto = new StockCardRangeSummaryDtoDataBuilder().build();
  }

  @Test
  public void shouldDeleteRequisitionIfItIsInitiated() {
    validateRequisitionDeleteWithStatus(INITIATED);
  }

  @Test
  public void shouldDeleteRequisitionWhenStatusIsSubmitted() {
    validateRequisitionDeleteWithStatus(SUBMITTED);
  }

  @Test
  public void shouldDeleteRequisitionWhenStatusIsSkipped() {
    validateRequisitionDeleteWithStatus(SKIPPED);
  }

  @Test
  public void shouldDeleteRequisitionIfRecentRequisitionIsLastInPeriod() {
    requisition.setStatus(INITIATED);
    stubRecentRequisition();

    requisitionService.delete(requisition);
    verify(requisitionRepository).delete(requisition);
  }

  @Test
  public void shouldDeleteRequisitionIfRecentRequisitionIsNotInLastPeriod() {
    requisition.setStatus(INITIATED);

    ProcessingPeriodDto secondPeriod = mock(ProcessingPeriodDto.class);
    when(secondPeriod.getId()).thenReturn(UUID.randomUUID());
    when(periodService.searchByProgramAndFacility(program.getId(), facility.getId()))
        .thenReturn(Arrays.asList(processingPeriod, secondPeriod));
    when(requisitionRepository
        .searchRequisitions(processingPeriod.getId(), facility.getId(), program.getId(), false))
        .thenReturn(singletonList(requisition));
    when(requisitionRepository
        .searchRequisitions(secondPeriod.getId(), facility.getId(), program.getId(), false))
        .thenReturn(Collections.emptyList());

    requisitionService.delete(requisition);
    verify(requisitionRepository).delete(requisition);
  }

  @Test
  public void shouldDeleteStatusMessagesWhenDeletingRequisition() {
    requisition.setStatus(INITIATED);
    List<StatusMessage> statusMessages = singletonList(
        StatusMessage.newStatusMessage(requisition, statusChange, null, null, null, "Message 1"));
    when(statusMessageRepository.findByRequisitionId(requisition.getId()))
        .thenReturn(statusMessages);
    stubRecentRequisition();

    requisitionService.delete(requisition);
    verify(requisitionRepository).delete(requisition);
    verify(statusMessageRepository).delete(statusMessages);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotDeleteRequisitionWhenStatusIsAuthorized() throws
      ValidationMessageException {
    requisition.setStatus(AUTHORIZED);
    stubRecentRequisition();

    requisitionService.delete(requisition);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotDeleteRequisitionWhenStatusIsInApproval() throws
      ValidationMessageException {
    requisition.setStatus(IN_APPROVAL);
    stubRecentRequisition();

    requisitionService.delete(requisition);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotDeleteRequisitionWhenStatusIsApproved() throws
      ValidationMessageException {
    requisition.setStatus(APPROVED);
    stubRecentRequisition();

    requisitionService.delete(requisition);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotDeleteRequisitionIfItIsNotNewest() {
    requisition.setStatus(INITIATED);

    prepareRequisitionIsNotNewest();

    requisitionService.delete(requisition);
  }

  @Test
  public void shouldDeleteEmergencyRequisitionEvenIfItIsNewest() {
    requisition.setStatus(INITIATED);
    requisition.setEmergency(true);
    prepareRequisitionIsNotNewest();

    requisitionService.delete(requisition);
    verify(requisitionRepository).delete(requisition);
  }

  @Test
  public void shouldRejectRequisitionIfRequisitionStatusIsAuthorized() {
    requisition.setStatus(AUTHORIZED);
    when(permissionService.canApproveRequisition(any(Requisition.class)))
        .thenReturn(ValidationResult.success());
    when(userRoleAssignmentsReferenceDataService.hasSupervisionRight(any(RightDto.class),
        any(UUID.class), any(UUID.class), any(UUID.class)))
        .thenReturn(true);
    Requisition returnedRequisition = requisitionService.reject(requisition, emptyMap());

    assertEquals(returnedRequisition.getStatus(), REJECTED);
  }

  @Test
  public void shouldRejectRequisitionIfRequisitionStatusIsInApproval() {
    requisition.setStatus(IN_APPROVAL);
    when(permissionService.canApproveRequisition(any(Requisition.class)))
        .thenReturn(ValidationResult.success());
    when(userRoleAssignmentsReferenceDataService.hasSupervisionRight(any(RightDto.class),
        any(UUID.class), any(UUID.class), any(UUID.class)))
        .thenReturn(true);
    Requisition returnedRequisition = requisitionService.reject(requisition, emptyMap());

    assertEquals(returnedRequisition.getStatus(), REJECTED);
  }

  @Test
  public void shouldSaveStatusMessageWhileRejectingRequisition() {
    requisition.setStatus(AUTHORIZED);
    requisition.setDraftStatusMessage("some_message");
    when(permissionService.canApproveRequisition(any(Requisition.class)))
        .thenReturn(ValidationResult.success());
    when(userRoleAssignmentsReferenceDataService.hasSupervisionRight(any(RightDto.class),
        any(UUID.class), any(UUID.class), any(UUID.class)))
        .thenReturn(true);
    Requisition returnedRequisition = requisitionService.reject(requisition, emptyMap());

    assertEquals(returnedRequisition.getStatus(), REJECTED);
    verify(statusMessageRepository, times(1)).save(any(StatusMessage.class));
  }

  @Test
  public void shouldSetNullToSupervisoryNodeWhileRejectingRequisition() {
    requisition.setStatus(AUTHORIZED);
    when(permissionService.canApproveRequisition(any(Requisition.class)))
        .thenReturn(ValidationResult.success());
    when(userRoleAssignmentsReferenceDataService.hasSupervisionRight(any(RightDto.class),
        any(UUID.class), any(UUID.class), any(UUID.class)))
        .thenReturn(true);
    Requisition returnedRequisition = requisitionService.reject(requisition, emptyMap());

    assertEquals(returnedRequisition.getStatus(), REJECTED);
    assertNull(returnedRequisition.getSupervisoryNodeId());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenRejectingRequisitionWithStatusSubmitted()
      throws ValidationMessageException {
    requisition.setStatus(SUBMITTED);
    requisitionService.reject(requisition, emptyMap());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenRejectingRequisitionWithStatusApproved()
      throws ValidationMessageException {
    requisition.setStatus(APPROVED);
    requisitionService.reject(requisition, emptyMap());
  }

  @Test
  public void shouldGetRequisitionsForApproval() {
    // given
    List<Requisition> requisitions = mockSearchRequisitionsForApproval();
    assertEquals(2, requisitions.size());

    DetailedRoleAssignmentDto detailedRoleAssignmentDto = mock(DetailedRoleAssignmentDto.class);
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(program.getId());
    when(detailedRoleAssignmentDto.getSupervisoryNodeId()).thenReturn(supervisoryNode.getId());
    when(detailedRoleAssignmentDto.getRole()).thenReturn(role);

    Set<RightDto> rights = new HashSet<>();
    rights.add(approveRequisitionRight);
    role.setRights(rights);

    Set<DetailedRoleAssignmentDto> roleAssignmentDtos = new HashSet<>();
    roleAssignmentDtos.add(detailedRoleAssignmentDto);
    UUID userId = UUID.randomUUID();
    when(userRoleAssignmentsReferenceDataService.getRoleAssignments(userId))
        .thenReturn(roleAssignmentDtos);

    // when
    Page<Requisition> requisitionsForApproval =
        requisitionService.getRequisitionsForApproval(userId, null, pageRequest);

    // then
    assertEquals(2, requisitionsForApproval.getTotalElements());
    assertTrue(requisitionsForApproval.getContent().contains(requisitions.get(0)));
    assertTrue(requisitionsForApproval.getContent().contains(requisitions.get(1)));
  }

  @Test
  public void shouldGetRequisitionsForApprovalWithProgramFilter() {
    List<Requisition> requisitions = mockSearchRequisitionsForApproval();
    assertEquals(2, requisitions.size());

    DetailedRoleAssignmentDto detailedRoleAssignmentDto = mock(DetailedRoleAssignmentDto.class);
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(program.getId());
    when(detailedRoleAssignmentDto.getSupervisoryNodeId()).thenReturn(supervisoryNode.getId());
    when(detailedRoleAssignmentDto.getRole()).thenReturn(role);

    Set<RightDto> rights = new HashSet<>();
    rights.add(approveRequisitionRight);
    role.setRights(rights);

    Set<DetailedRoleAssignmentDto> roleAssignmentDtos = new HashSet<>();
    roleAssignmentDtos.add(detailedRoleAssignmentDto);
    when(userRoleAssignmentsReferenceDataService.getRoleAssignments(user.getId()))
            .thenReturn(roleAssignmentDtos);

    Page<Requisition> requisitionsForApproval = requisitionService
                .getRequisitionsForApproval(user.getId(), program.getId(), pageRequest);

    assertEquals(2, requisitionsForApproval.getTotalElements());
    assertTrue(requisitionsForApproval.getContent().contains(requisitions.get(0)));
    assertTrue(requisitionsForApproval.getContent().contains(requisitions.get(1)));
  }

  @Test
  public void shouldNotGetRequisitionsForApprovalWithoutApproveRight() {
    DetailedRoleAssignmentDto detailedRoleAssignmentDto = mock(DetailedRoleAssignmentDto.class);
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(program.getId());
    when(detailedRoleAssignmentDto.getSupervisoryNodeId()).thenReturn(supervisoryNode.getId());
    when(detailedRoleAssignmentDto.getRole()).thenReturn(role);

    Set<RightDto> rights = new HashSet<>();
    role.setRights(rights);

    Set<DetailedRoleAssignmentDto> roleAssignmentDtos = new HashSet<>();
    roleAssignmentDtos.add(detailedRoleAssignmentDto);
    when(userRoleAssignmentsReferenceDataService.getRoleAssignments(user.getId()))
        .thenReturn(roleAssignmentDtos);
    when(requisitionRepository.searchApprovableRequisitionsByProgramSupervisoryNodePairs(
        any(Set.class), any(Pageable.class)))
        .thenReturn(getPage(Collections.emptyList(), pageRequest));

    Page<Requisition> requisitionsForApproval =
        requisitionService.getRequisitionsForApproval(user.getId(), null, pageRequest);

    assertEquals(0, requisitionsForApproval.getTotalElements());
  }

  @Test
  public void shouldNotGetRequisitionsForApprovalWithIncorrectSupervisoryNode() {
    DetailedRoleAssignmentDto detailedRoleAssignmentDto = mock(DetailedRoleAssignmentDto.class);
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(program.getId());
    when(detailedRoleAssignmentDto.getSupervisoryNodeId()).thenReturn(UUID.randomUUID());
    when(detailedRoleAssignmentDto.getRole()).thenReturn(role);

    Set<RightDto> rights = new HashSet<>();
    rights.add(approveRequisitionRight);
    role.setRights(rights);

    Set<DetailedRoleAssignmentDto> roleAssignmentDtos = new HashSet<>();
    roleAssignmentDtos.add(detailedRoleAssignmentDto);
    when(userRoleAssignmentsReferenceDataService.getRoleAssignments(user.getId()))
        .thenReturn(roleAssignmentDtos);
    when(requisitionRepository.searchApprovableRequisitionsByProgramSupervisoryNodePairs(
        any(Set.class), any(Pageable.class)))
        .thenReturn(getPage(Collections.emptyList(), pageRequest));

    Page<Requisition> requisitionsForApproval =
        requisitionService.getRequisitionsForApproval(user.getId(), null, pageRequest);

    assertEquals(0, requisitionsForApproval.getTotalElements());
  }

  @Test
  public void shouldNotGetRequisitionsForApprovalWithIncorrectProgram() {
    DetailedRoleAssignmentDto detailedRoleAssignmentDto = mock(DetailedRoleAssignmentDto.class);
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(UUID.randomUUID());
    when(detailedRoleAssignmentDto.getSupervisoryNodeId()).thenReturn(supervisoryNode.getId());
    when(detailedRoleAssignmentDto.getRole()).thenReturn(role);

    Set<RightDto> rights = new HashSet<>();
    rights.add(approveRequisitionRight);
    role.setRights(rights);

    Set<DetailedRoleAssignmentDto> roleAssignmentDtos = new HashSet<>();
    roleAssignmentDtos.add(detailedRoleAssignmentDto);
    when(userRoleAssignmentsReferenceDataService.getRoleAssignments(user.getId()))
        .thenReturn(roleAssignmentDtos);
    when(requisitionRepository.searchApprovableRequisitionsByProgramSupervisoryNodePairs(
        any(Set.class), any(Pageable.class)))
        .thenReturn(getPage(Collections.emptyList(), pageRequest));

    Page<Requisition> requisitionsForApproval =
        requisitionService.getRequisitionsForApproval(user.getId(), null, pageRequest);

    assertEquals(0, requisitionsForApproval.getTotalElements());
  }

  @Test
  public void shouldPassValidationIfUserCanApproveRequisition() {
    when(permissionService.canApproveRequisition(any(Requisition.class)))
        .thenReturn(ValidationResult.success());
    when(userRoleAssignmentsReferenceDataService.hasSupervisionRight(any(RightDto.class),
        any(UUID.class), any(UUID.class), any(UUID.class)))
        .thenReturn(true);

    ValidationResult result = requisitionService.validateCanApproveRequisition(
        requisition, user.getId());

    assertTrue(result.isSuccess());
  }

  @Test
  public void shouldFailValidationIfUserHasNoApproveRightAssigned() {
    when(permissionService.canApproveRequisition(any(Requisition.class)))
        .thenReturn(ValidationResult.noPermission("no.permission"));

    ValidationResult result = requisitionService.validateCanApproveRequisition(requisition,
        user.getId());

    assertTrue(result.hasErrors());
    assertEquals(FailureType.NO_PERMISSION, result.getError().getType());
  }

  @Test
  public void shouldFailValidationIfRequisitionIsInIncorrectState() {
    when(permissionService.canApproveRequisition(any(Requisition.class)))
        .thenReturn(ValidationResult.success());
    requisition.setStatus(INITIATED);

    ValidationResult result = requisitionService.validateCanApproveRequisition(requisition,
        user.getId());

    assertTrue(result.hasErrors());
    assertEquals(FailureType.VALIDATION, result.getError().getType());
  }

  @Test
  public void shouldFailValidationIfUserHasNoUpdateRightsAssigned() {
    when(permissionService.canUpdateRequisition(any(Requisition.class)))
        .thenReturn(ValidationResult.noPermission("no.permission"));

    ValidationResult result = requisitionService.validateCanSaveRequisition(requisitionDto.getId());

    assertTrue(result.hasErrors());
    assertEquals(FailureType.NO_PERMISSION, result.getError().getType());
  }

  @Test
  public void shouldFailValidationIfRequisitionDoesNotExistOnSave() {
    when(permissionService.canUpdateRequisition(any(Requisition.class)))
        .thenReturn(ValidationResult.success());
    when(requisitionRepository.findOne(requisitionDto.getId())).thenReturn(null);

    ValidationResult result = requisitionService.validateCanSaveRequisition(requisitionDto.getId());

    assertTrue(result.hasErrors());
    assertEquals(FailureType.NOT_FOUND, result.getError().getType());
  }

  @Test
  public void shouldFailValidationIfRequisitionIsInIncorrectStatus() {
    when(permissionService.canUpdateRequisition(any(Requisition.class)))
        .thenReturn(ValidationResult.success());
    when(requisitionRepository.findOne(requisitionDto.getId())).thenReturn(requisition);
    requisition.setStatus(RELEASED);

    ValidationResult result = requisitionService.validateCanSaveRequisition(requisitionDto.getId());

    assertTrue(result.hasErrors());
    assertEquals(FailureType.VALIDATION, result.getError().getType());
  }

  @Test
  public void shouldPassValidationIfUserCanUpdateRequisition() {
    when(permissionService.canUpdateRequisition(any(Requisition.class)))
        .thenReturn(ValidationResult.success());
    when(requisitionRepository.findOne(requisitionDto.getId())).thenReturn(requisition);
    requisition.setStatus(SUBMITTED);

    ValidationResult result = requisitionService.validateCanSaveRequisition(requisitionDto.getId());

    assertTrue(result.isSuccess());
  }

  @Test
  public void shouldInitiateRequisitionIfItDoesNotAlreadyExist() {
    prepareForTestInitiate(SETTING);
    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    when(requisitionTemplate.getNumberOfPeriodsToAverage()).thenReturn(2);

    when(requisitionRepository
        .findOne(requisition.getId()))
        .thenReturn(null);

    ProcessingPeriodDto periodDto = new ProcessingPeriodDto();
    periodDto.setStartDate(LocalDate.of(2016, 11, 1));
    periodDto.setEndDate(LocalDate.of(2016, 11, 30));
    periodDto.setDurationInMonths(1);
    doReturn(periodDto)
        .when(periodService)
        .findPeriod(program.getId(), facility.getId(), processingPeriod.getId(), false);

    Requisition initiatedRequisition = requisitionService.initiate(
        program, facility, processingPeriod, false,
        stockAdjustmentReasons, requisitionTemplate);

    assertEquals(INITIATED, initiatedRequisition.getStatus());
    assertEquals(1, initiatedRequisition.getNumberOfMonthsInPeriod().longValue());
  }

  @Test
  public void shouldInitiatePreviousAdjustedConsumptions() {
    prepareForTestInitiate(SETTING);
    when(periodService.findPreviousPeriods(any(), eq(SETTING - 1)))
        .thenReturn(singletonList(new ProcessingPeriodDto()));
    mockPreviousRequisition();
    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        program, facility, processingPeriod, false,
        stockAdjustmentReasons, requisitionTemplate);

    RequisitionLineItem requisitionLineItem = initiatedRequisition.getRequisitionLineItems().get(0);
    assertEquals(Integer.valueOf(ADJUSTED_CONSUMPTION),
        requisitionLineItem.getPreviousAdjustedConsumptions().get(0));
  }

  @Test
  public void shouldInitiateReportOnlyRequisition() {
    prepareForTestInitiate(SETTING);
    when(periodService.findPreviousPeriods(any(), eq(SETTING - 1)))
        .thenReturn(singletonList(new ProcessingPeriodDto()));
    mockPreviousRequisition();
    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    processingPeriod.setExtraData(ImmutableMap.of("reportOnly", "true"));

    Requisition initiatedRequisition = requisitionService.initiate(
        program, facility, processingPeriod, false,
        stockAdjustmentReasons, requisitionTemplate);

    assertTrue(initiatedRequisition.getReportOnly());
  }

  @Test
  public void shouldInitiateRegularRequisitionIfItIsEmergencyForReportOnlyPeriod() {
    prepareForTestInitiate(SETTING);
    when(periodService.findPreviousPeriods(any(), eq(SETTING - 1)))
        .thenReturn(singletonList(new ProcessingPeriodDto()));
    mockPreviousRequisition();
    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    processingPeriod.setExtraData(ImmutableMap.of("reportOnly", "true"));

    Requisition initiatedRequisition = requisitionService.initiate(
        program, facility, processingPeriod, true,
        stockAdjustmentReasons, requisitionTemplate);

    assertFalse(initiatedRequisition.getReportOnly());
  }

  @Test
  public void shouldInitiatePreviousAdjustedConsumptionsBasedOnRegularRequisitions() {
    prepareForTestInitiate(SETTING);
    stubPreviousPeriod();
    mockPreviousRequisition();
    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        program, facility, processingPeriod, false,
        stockAdjustmentReasons, requisitionTemplate);

    RequisitionLineItem requisitionLineItem = initiatedRequisition.getRequisitionLineItems().get(0);
    assertEquals(Integer.valueOf(ADJUSTED_CONSUMPTION),
        requisitionLineItem.getPreviousAdjustedConsumptions().get(0));
    verify(requisitionRepository).searchRequisitions(
        PERIOD_ID, facility.getId(), program.getId(), false);
  }

  @Test
  public void shouldAssignPreviousRegularRequisition() {
    prepareForTestInitiate(SETTING);
    stubPreviousPeriod();
    mockPreviousRequisition();
    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        program, facility, processingPeriod, false,
        stockAdjustmentReasons, requisitionTemplate);

    UUID previousRequisitionId = initiatedRequisition.getPreviousRequisitions().get(0).getId();
    assertEquals(previousRequisition.getId(), previousRequisitionId);
    verify(requisitionRepository).searchRequisitions(
        PERIOD_ID, facility.getId(), program.getId(), false);
  }

  @Test
  public void shouldAssignIdealStockAmount() {
    prepareForTestInitiate(SETTING);
    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    when(idealStockAmountReferenceDataService.search(any(UUID.class), any(UUID.class)))
        .thenReturn(Lists.newArrayList(new IdealStockAmountDtoDataBuilder()
            .withCommodityTypeId(COMMODITY_TYPE_ID)
            .build()));

    Requisition initiatedRequisition = requisitionService.initiate(
        program, facility, processingPeriod, false,
        stockAdjustmentReasons, requisitionTemplate);

    List<RequisitionLineItem> lineItems = initiatedRequisition.getRequisitionLineItems();
    assertThat(lineItems, hasSize(1));
    assertThat(lineItems.get(0).getIdealStockAmount(), is(1000));
  }

  @Test
  public void shouldSetEmptyPreviousAdjustedConsumptionsWhenNumberOfPeriodsToAverageIsNull() {
    prepareForTestInitiate(null);
    mockPreviousRequisition();
    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        this.program, facility, processingPeriod, false,
        stockAdjustmentReasons, requisitionTemplate);

    verify(periodService).findPreviousPeriods(any(), eq(1));
    RequisitionLineItem requisitionLineItem = initiatedRequisition.getRequisitionLineItems().get(0);
    assertEquals(0, requisitionLineItem.getPreviousAdjustedConsumptions().size());
  }

  @Test
  public void shouldNotSetPreviousAdjustedConsumptionsIfNoRequisitionForNoPreviousRequisition() {
    prepareForTestInitiate(SETTING);
    when(periodService.findPreviousPeriods(any(), eq(SETTING - 1)))
        .thenReturn(singletonList(new ProcessingPeriodDto()));
    mockNoPreviousRequisition();
    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        this.program, facility, processingPeriod, false,
        stockAdjustmentReasons, requisitionTemplate);

    RequisitionLineItem requisitionLineItem = initiatedRequisition.getRequisitionLineItems().get(0);
    assertEquals(0, requisitionLineItem.getPreviousAdjustedConsumptions().size());
  }

  @Test
  public void shouldNotSetPreviousAdjustedConsumptionsIfNoRequisitionForNoPreviousPeriod() {
    prepareForTestInitiate(SETTING);
    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        this.program, facility, processingPeriod, false,
        stockAdjustmentReasons, requisitionTemplate);

    RequisitionLineItem requisitionLineItem = initiatedRequisition.getRequisitionLineItems().get(0);
    assertEquals(0, requisitionLineItem.getPreviousAdjustedConsumptions().size());
  }

  @Test
  public void shouldSetStockAdjustmenReasonsDuringInitiate() {
    prepareForTestInitiate(SETTING);
    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        program, facility, processingPeriod, false,
        stockAdjustmentReasons, requisitionTemplate);

    assertEquals(stockAdjustmentReasons, initiatedRequisition.getStockAdjustmentReasons());
  }

  @Test
  public void shouldPopulateOnlyNonFullProductsDuringInitiateForRegularRequisition() {
    prepareForTestInitiate(SETTING);
    mockApprovedProduct(new UUID[]{PRODUCT_ID, NON_FULL_PRODUCT_ID}, new boolean[]{true, false});

    Requisition initiatedRequisition = requisitionService.initiate(
        this.program, facility, processingPeriod, false,
        stockAdjustmentReasons, requisitionTemplate);

    Set<UUID> availableProducts = initiatedRequisition.getAvailableProducts();
    assertThat(availableProducts, hasSize(1));
    assertThat(availableProducts, hasItems(NON_FULL_PRODUCT_ID));
  }

  @Test
  public void shouldPopulateFullAndNonFullProductsDuringInitiateForEmergencyRequisition() {
    prepareForTestInitiate(SETTING);
    mockApprovedProduct(new UUID[]{PRODUCT_ID, NON_FULL_PRODUCT_ID}, new boolean[]{true, false});

    Requisition initiatedRequisition = requisitionService.initiate(
        this.program, facility, processingPeriod, true,
        stockAdjustmentReasons, requisitionTemplate);

    Set<UUID> availableProducts = initiatedRequisition.getAvailableProducts();
    assertThat(availableProducts, hasSize(2));
    assertThat(availableProducts, hasItems(PRODUCT_ID, NON_FULL_PRODUCT_ID));
  }

  @Test
  public void shouldSetStockOnHandFromStockIfFlagIsEnabled() {
    prepareForGetStockOnHandTest();
    stockCardSummaryDto.setStockOnHand(10);

    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        program, facility, processingPeriod, false, stockAdjustmentReasons, requisitionTemplate);

    assertEquals(stockCardSummaryDto.getStockOnHand(),
        initiatedRequisition.getRequisitionLineItems().get(0).getStockOnHand());
  }

  @Test
  public void shouldNotSetStockOnHandIfFlagIsDisabled() {
    prepareForTestInitiate(SETTING);
    when(requisitionTemplate.isPopulateStockOnHandFromStockCards()).thenReturn(false);
    whenGetStockCardSummaries().thenThrow(IllegalStateException.class);

    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        program, facility, processingPeriod, false, stockAdjustmentReasons, requisitionTemplate);

    assertNull(initiatedRequisition.getRequisitionLineItems().get(0).getStockOnHand());
    verifyZeroInteractions(stockCardRangeSummaryStockManagementService);
  }

  @Test
  public void shouldNotIncludeLineItemsIfNoStockCardSummariesFound() {
    prepareForTestInitiate(SETTING);
    when(requisitionTemplate.isPopulateStockOnHandFromStockCards()).thenReturn(true);
    whenGetStockCardRangeSummaries().thenReturn(singletonList(stockCardRangeSummaryDto));
    whenGetStockCardSummaries().thenReturn(Collections.emptyList());

    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        program, facility, processingPeriod, false, stockAdjustmentReasons, requisitionTemplate);

    assertTrue(initiatedRequisition.getRequisitionLineItems().isEmpty());
  }

  @Test
  public void shouldReleaseRequisitionsAsOrder() {
    // given
    List<ReleasableRequisitionDto> requisitions = setUpReleaseRequisitionsAsOrder(5,
        APPROVED);
    List<FacilityDto> facilities = requisitions.stream()
        .map(r -> facilityReferenceDataService.findOne(r.getSupplyingDepotId()))
        .collect(Collectors.toList());

    when(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(user.getId(),
        convertToOrderRight.getId())).thenReturn(facilities);
    when(requisitionForConvertBuilder.getAvailableSupplyingDepots(any(UUID.class)))
        .thenReturn(facilities);

    // when
    List<Requisition> expectedRequisitions = requisitionService
        .convertToOrder(requisitions, user);

    // then
    for (Requisition requisition : expectedRequisitions) {
      assertEquals(RELEASED, requisition.getStatus());
    }
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotReleaseRequisitionsAsOrderIfSupplyingDepotsNotProvided() {
    // given
    List<ReleasableRequisitionDto> requisitions = setUpReleaseRequisitionsAsOrder(5,
        APPROVED);
    List<FacilityDto> facilities = requisitions.stream()
        .map(r -> facilityReferenceDataService.findOne(r.getSupplyingDepotId()))
        .collect(Collectors.toList());

    when(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(user.getId(),
        convertToOrderRight.getId())).thenReturn(facilities);

    for (ReleasableRequisitionDto requisition : requisitions) {
      requisition.setSupplyingDepotId(null);
    }

    // when
    requisitionService.convertToOrder(requisitions, user);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotReleaseRequisitionsAsOrderIfUserHasNoFulfillmentRightsForFacility() {
    // given
    List<ReleasableRequisitionDto> requisitions = setUpReleaseRequisitionsAsOrder(5,
        APPROVED);

    when(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(user.getId(),
        convertToOrderRight.getId())).thenReturn(new ArrayList<>());

    // when
    requisitionService.convertToOrder(requisitions, user);
  }


  @Test(expected = ValidationMessageException.class)
  public void shouldNotReleaseRequisitionsAsOrderIfApprovedQtyDisabled() {
    // given
    List<ReleasableRequisitionDto> requisitions = setUpReleaseRequisitionsAsOrder(1,
        APPROVED);
    when(requisitionTemplate.isColumnInTemplateAndDisplayed(APPROVED_QUANTITY)).thenReturn(false);

    // when
    requisitionService.convertToOrder(requisitions, user);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotReleaseRequisitionsAsOrderInIncorrectStatus() {
    // given
    List<ReleasableRequisitionDto> requisitions = setUpReleaseRequisitionsAsOrder(1,
        SUBMITTED);

    // when
    requisitionService.convertToOrder(requisitions, user);
  }

  @Test
  public void shouldCallApproveRequisition() {
    OrderableDto fullSupplyOrderable = new OrderableDtoDataBuilder().build();
    SupplyLineDto supplyLineDto = new SupplyLineDtoDataBuilder().build();
    when(orderableReferenceDataService.findByIds(any())).thenReturn(
        singletonList(fullSupplyOrderable));

    UUID parentId = UUID.randomUUID();

    requisitionService.doApprove(
        parentId, user, ImmutableMap.of(fullSupplyOrderable.getId(), fullSupplyOrderable),
        requisitionMock, singletonList(supplyLineDto)
    );

    verify(requisitionMock, times(1)).approve(eq(parentId),
        eq(ImmutableMap.of(fullSupplyOrderable.getId(), fullSupplyOrderable)),
        eq(singletonList(supplyLineDto)), eq(user.getId()));
  }

  @Test
  public void shouldFindRequisitionIfItExists() {

    when(requisitionRepository.searchRequisitions(
        requisition.getFacilityId(),
        requisition.getProgramId(),
        requisition.getCreatedDate().minusDays(2).toLocalDate(),
        requisition.getCreatedDate().plusDays(2).toLocalDate(),
        requisition.getProcessingPeriodId(),
        requisition.getSupervisoryNodeId(),
        EnumSet.of(requisition.getStatus()),
        null,
        permissionStrings,
        pageRequest))
        .thenReturn(getPage(singletonList(requisition), pageRequest));

    List<Requisition> receivedRequisitions = requisitionService.searchRequisitions(
        requisition.getFacilityId(),
        requisition.getProgramId(),
        requisition.getCreatedDate().minusDays(2).toLocalDate(),
        requisition.getCreatedDate().plusDays(2).toLocalDate(),
        requisition.getProcessingPeriodId(),
        requisition.getSupervisoryNodeId(),
        EnumSet.of(requisition.getStatus()),
        null,
        pageRequest).getContent();

    assertEquals(1, receivedRequisitions.size());
    assertEquals(
        receivedRequisitions.get(0).getFacilityId(),
        requisition.getFacilityId());
    assertEquals(
        receivedRequisitions.get(0).getProgramId(),
        requisition.getProgramId());
    assertTrue(
        receivedRequisitions.get(0).getCreatedDate().isAfter(
            requisition.getCreatedDate().minusDays(2)));
    assertTrue(
        receivedRequisitions.get(0).getCreatedDate().isBefore(
            requisition.getCreatedDate().plusDays(2)));
    assertEquals(
        receivedRequisitions.get(0).getProcessingPeriodId(),
        requisition.getProcessingPeriodId());
    assertEquals(
        receivedRequisitions.get(0).getSupervisoryNodeId(),
        requisition.getSupervisoryNodeId());
    assertEquals(
        receivedRequisitions.get(0).getStatus(),
        requisition.getStatus());
  }

  @Test
  public void searchShouldReturnEmptyListIfPermissionStringsIsEmpty() {
    // given
    when(permissionService.getPermissionStrings()).thenReturn(Collections.emptyList());

    // when
    List<Requisition> receivedRequisitions = requisitionService.searchRequisitions(
        requisition.getFacilityId(),
        requisition.getProgramId(),
        requisition.getCreatedDate().minusDays(2).toLocalDate(),
        requisition.getCreatedDate().plusDays(2).toLocalDate(),
        requisition.getProcessingPeriodId(),
        requisition.getSupervisoryNodeId(),
        EnumSet.of(requisition.getStatus()),
        null,
        pageRequest).getContent();

    // then
    assertEquals(0, receivedRequisitions.size());
  }

  @Test
  public void shouldFindAndSortAndPageApprovedRequisitions() {
    // given
    List<BasicRequisitionDto> requisitionDtos = getBasicRequisitionDtoList();

    String filterAndSortBy = "programName";

    UUID supplyingDepotId = UUID.randomUUID();
    List<FacilityDto> supplyingDepots = mockSupplyingDepot(supplyingDepotId);

    int pageSize = 3;
    int pageNumber = 0;
    Pageable pageable = mockPageable();


    setupStubsForTestApprovedRequisition(requisitionDtos, filterAndSortBy, filterAndSortBy,
        null, null, supplyingDepots, pageable, pageSize, pageNumber);

    List<BasicRequisitionDto> requisitionDtosSubList =
        requisitionDtos.subList(pageNumber * pageSize, pageNumber * pageSize + pageSize);

    List<RequisitionWithSupplyingDepotsDto> requisitionWithSupplyingDepotDtos =
        requisitionDtosSubList
            .stream()
            .map(requisitionDto ->
                new RequisitionWithSupplyingDepotsDto(requisitionDto, supplyingDepots))
            .collect(Collectors.toList());

    //when
    Page<RequisitionWithSupplyingDepotsDto> requisitionDtosRetrieved =
        requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(null,
            filterAndSortBy, pageable, Arrays.asList(supplyingDepotId));

    List<RequisitionWithSupplyingDepotsDto> requisitionDtosRetrievedList =
        requisitionDtosRetrieved.getContent();

    //then
    for (int i = 0; i < requisitionDtosRetrievedList.size(); i++) {
      assertEquals(
          requisitionWithSupplyingDepotDtos.get(i).getRequisition(),
          requisitionDtosRetrievedList.get(i).getRequisition()
      );
      assertEquals(
          requisitionWithSupplyingDepotDtos.get(i).getSupplyingDepots(),
          requisitionDtosRetrievedList.get(i).getSupplyingDepots()
      );
    }
    assertEquals(requisitionDtos.size(), requisitionDtosRetrieved.getTotalElements());
    assertEquals(pageSize, requisitionDtosRetrieved.getNumberOfElements());
    assertTrue(requisitionDtosRetrieved.isFirst());
    assertFalse(requisitionDtosRetrieved.isLast());
  }

  @Test
  public void shouldFilterRequisitionsForConvertByMultipleProgramValues() {
    // given
    final String filterBy = "programName";
    final String fpProgram = "Family Planning";
    final String emProgram = "Essential Meds";
    final String em = "Essential";

    UUID supplyingDepotId = UUID.randomUUID();
    List<FacilityDto> supplyingDepots = mockSupplyingDepot(supplyingDepotId);

    int pageSize = 20;
    int pageNumber = 0;
    Pageable pageable = mockPageable();

    List<BasicRequisitionDto> essentialMedsRequisitions = getBasicRequisitionDtoList();
    List<BasicRequisitionDto> familyPlanningRequisitions = getBasicRequisitionDtoList();

    setupStubsForTestApprovedRequisition(essentialMedsRequisitions, filterBy, emProgram,
        null, null, supplyingDepots, pageable, pageSize, pageNumber);
    setupStubsForTestApprovedRequisition(familyPlanningRequisitions, filterBy, fpProgram,
        null, null, supplyingDepots, pageable, pageSize, pageNumber);
    setupStubsForTestApprovedRequisition(essentialMedsRequisitions, filterBy, em,
        null, null, supplyingDepots, pageable, pageSize, pageNumber);

    when(programReferenceDataService.search(emProgram)).thenReturn(Lists.newArrayList(program));
    when(programReferenceDataService.search(em)).thenReturn(Lists.newArrayList(program));

    //when
    requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        Lists.newArrayList(emProgram, em, fpProgram), filterBy, pageable,
        Arrays.asList(supplyingDepotId));

    // then
    verify(programReferenceDataService).search(emProgram);
    verify(programReferenceDataService).search(em);
    verify(programReferenceDataService).search(fpProgram);
    verify(facilityReferenceDataService, never())
        .search(anyString(), anyString(), any(UUID.class), anyBoolean());
  }

  @Test
  public void shouldFilterRequisitionsForConvertByMultipleFacilityCodeValues() {
    // given
    final String filterBy = "facilityCode";
    final String code1 = "LL001";
    final String code2 = "LL002";

    UUID supplyingDepotId = UUID.randomUUID();
    List<FacilityDto> supplyingDepots = mockSupplyingDepot(supplyingDepotId);

    int pageSize = 20;
    int pageNumber = 0;
    Pageable pageable = mockPageable();

    setupStubsForTestApprovedRequisition(getBasicRequisitionDtoList(), filterBy, null,
        code1, null, supplyingDepots, pageable, pageSize, pageNumber);
    setupStubsForTestApprovedRequisition(getBasicRequisitionDtoList(), filterBy, null,
        code2, null, supplyingDepots, pageable, pageSize, pageNumber);

    //when
    requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        Lists.newArrayList(code1, code2), filterBy, pageable, Arrays.asList(supplyingDepotId));

    // then
    verify(facilityReferenceDataService).search(code1, null, null, false);
    verify(facilityReferenceDataService).search(code2, null, null, false);
    verify(programReferenceDataService).findAll();
  }

  @Test
  public void shouldFilterRequisitionsForConvertByMultipleFacilityNameValues() {
    // given
    final String filterBy = "facilityName";
    final String name1 = "Comfort Health Clinic";
    final String name2 = "Balaka District Hospital";

    UUID supplyingDepotId = UUID.randomUUID();
    List<FacilityDto> supplyingDepots = mockSupplyingDepot(supplyingDepotId);

    int pageSize = 20;
    int pageNumber = 0;
    Pageable pageable = mockPageable();

    setupStubsForTestApprovedRequisition(getBasicRequisitionDtoList(), filterBy, null,
        null, name1, supplyingDepots, pageable, pageSize, pageNumber);
    setupStubsForTestApprovedRequisition(getBasicRequisitionDtoList(), filterBy, null,
        null, name2, supplyingDepots, pageable, pageSize, pageNumber);

    //when
    requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        Lists.newArrayList(name1, name2), filterBy, pageable, Arrays.asList(supplyingDepotId));

    // then
    verify(facilityReferenceDataService).search(null, name1, null, false);
    verify(facilityReferenceDataService).search(null, name2, null, false);
    verify(programReferenceDataService).findAll();
  }

  @Test
  public void shouldFilterRequisitionsForConvertByMultipleValues() {
    // given
    final String filterBy = "all";
    final String expression1 = "Essential";
    final String expression2 = "Clinic";

    UUID supplyingDepotId = UUID.randomUUID();
    List<FacilityDto> supplyingDepots = mockSupplyingDepot(supplyingDepotId);

    int pageSize = 20;
    int pageNumber = 0;
    Pageable pageable = mockPageable();

    setupStubsForTestApprovedRequisition(getBasicRequisitionDtoList(), filterBy, expression1,
        expression1, expression1, supplyingDepots, pageable, pageSize, pageNumber);
    setupStubsForTestApprovedRequisition(getBasicRequisitionDtoList(), filterBy, expression2,
        expression2, expression2, supplyingDepots, pageable, pageSize, pageNumber);

    //when
    requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        Lists.newArrayList(expression1, expression2), filterBy, pageable,
        Arrays.asList(supplyingDepotId));

    // then
    verify(facilityReferenceDataService).search(expression1, expression1, null, false);
    verify(facilityReferenceDataService).search(expression2, expression2, null, false);
    verify(programReferenceDataService).search(expression1);
    verify(programReferenceDataService).search(expression2);
  }

  @Test
  public void shouldNotFilterRequisitionsForConvertIfNoFilterValueProvided() {
    // given
    final String filterBy = "all";

    UUID supplyingDepotId = UUID.randomUUID();
    List<FacilityDto> supplyingDepots = mockSupplyingDepot(supplyingDepotId);

    int pageSize = 20;
    int pageNumber = 0;
    Pageable pageable = mockPageable();

    setupStubsForTestApprovedRequisition(getBasicRequisitionDtoList(), filterBy, null,
        null, null, supplyingDepots, pageable, pageSize, pageNumber);

    //when
    requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(null, filterBy,
        pageable, Arrays.asList(supplyingDepotId));

    // then
    verify(facilityReferenceDataService).findAll();
    verify(programReferenceDataService).findAll();
  }

  @Test
  public void shouldConvertRequisitionsToOrders() {
    // given
    int requisitionsCount = 5;

    List<ReleasableRequisitionDto> list = setUpReleaseRequisitionsAsOrder(requisitionsCount,
        APPROVED);

    List<FacilityDto> facilities = list.stream()
        .map(r -> facilityReferenceDataService.findOne(r.getSupplyingDepotId()))
        .collect(Collectors.toList());

    when(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(user.getId(),
        convertToOrderRight.getId())).thenReturn(facilities);
    when(requisitionForConvertBuilder.getAvailableSupplyingDepots(any(UUID.class)))
        .thenReturn(facilities);

    // when
    requisitionService.convertToOrder(list, user);

    // then
    verify(orderFulfillmentService).create(any(List.class));
  }


  @Test(expected = ValidationMessageException.class)
  public void shouldNotSwallowExceptionsFromFulfillmentService() {
    // given
    int requisitionsCount = 5;

    List<ReleasableRequisitionDto> list = setUpReleaseRequisitionsAsOrder(requisitionsCount,
        APPROVED);

    List<FacilityDto> facilities = list.stream()
        .map(r -> facilityReferenceDataService.findOne(r.getSupplyingDepotId()))
        .collect(Collectors.toList());

    when(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(user.getId(),
        convertToOrderRight.getId())).thenReturn(facilities);

    doThrow(new ValidationMessageException(("test"))).when(
      orderFulfillmentService).create(any(List.class));

    // when
    requisitionService.convertToOrder(list, user);
  }

  @Test
  public void shouldProcessStatusChangeWhenConvertingRequisitionToOrder() throws Exception {
    List<ReleasableRequisitionDto> list = setUpReleaseRequisitionsAsOrder(1, APPROVED);

    List<FacilityDto> facilities = list.stream()
        .map(r -> facilityReferenceDataService.findOne(r.getSupplyingDepotId()))
        .collect(Collectors.toList());

    when(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(user.getId(),
        convertToOrderRight.getId())).thenReturn(facilities);
    when(requisitionForConvertBuilder.getAvailableSupplyingDepots(any(UUID.class)))
        .thenReturn(facilities);

    requisitionService.convertToOrder(list, user);

    verify(requisitionStatusProcessor).statusChange(any(Requisition.class));
  }

  @Test
  public void shouldSaveStatusMessage() {
    // given
    Requisition requisition = generateRequisition();
    StatusChange statusChange = new StatusChange();
    statusChange.setRequisition(requisition);

    StatusChange anotherStatusChange = new StatusChange();
    anotherStatusChange.setId(UUID.randomUUID());
    anotherStatusChange.setCreatedDate(ZonedDateTime.now());

    requisition.setStatusChanges(Arrays.asList(statusChange, anotherStatusChange));
    requisition.setDraftStatusMessage("some_message");

    // when
    requisitionService.saveStatusMessage(requisition, user);

    // then
    ArgumentCaptor<StatusMessage> captor = ArgumentCaptor.forClass(StatusMessage.class);
    verify(statusMessageRepository).save(captor.capture());
    StatusMessage savedMessage = captor.getValue();
    assertEquals(statusChange, savedMessage.getStatusChange());
    assertEquals("", requisition.getDraftStatusMessage());
  }

  @Test
  public void shouldSaveStatusMessageWithAStatusChangeThatWasAlreadyPersisted() {
    // given
    Requisition requisition = generateRequisition();
    StatusChange statusChange = new StatusChange();
    statusChange.setId(UUID.randomUUID());
    statusChange.setRequisition(requisition);
    statusChange.setCreatedDate(ZonedDateTime.now().minusDays(1));

    StatusChange anotherStatusChange = new StatusChange();
    anotherStatusChange.setId(UUID.randomUUID());
    statusChange.setRequisition(requisition);
    anotherStatusChange.setCreatedDate(ZonedDateTime.now());

    requisition.setStatusChanges(Arrays.asList(statusChange, anotherStatusChange));
    requisition.setDraftStatusMessage("some_message");

    // when
    requisitionService.saveStatusMessage(requisition, user);

    // then
    ArgumentCaptor<StatusMessage> captor = ArgumentCaptor.forClass(StatusMessage.class);
    verify(statusMessageRepository).save(captor.capture());
    StatusMessage savedMessage = captor.getValue();
    assertEquals(anotherStatusChange, savedMessage.getStatusChange());
    assertEquals("", requisition.getDraftStatusMessage());
  }

  @Test
  public void shouldNotSaveStatusMessageIfDraftIsNullOrEmpty() {
    // given
    Requisition requisition = generateRequisition();
    requisition.setDraftStatusMessage(null);

    // when
    requisitionService.saveStatusMessage(requisition, user);

    requisition.setDraftStatusMessage("");
    requisitionService.saveStatusMessage(requisition, user);

    // then
    verify(statusMessageRepository, never()).save(any(StatusMessage.class));
  }

  @Test
  public void shouldReleaseRequisitionsWithoutOrder() {
    // given
    List<ReleasableRequisitionDto> requisitions = setUpReleaseRequisitionsAsOrder(5,
        APPROVED);

    // when
    List<Requisition> expectedRequisitions = requisitionService
        .releaseWithoutOrder(requisitions);

    // then
    for (Requisition requisition : expectedRequisitions) {
      assertEquals(RELEASED_WITHOUT_ORDER, requisition.getStatus());
    }
  }

  @Test
  public void shouldReleaseRequisitionsWithoutOrderIfApprovedQtyDisabled() {
    // given
    List<ReleasableRequisitionDto> requisitions = setUpReleaseRequisitionsAsOrder(1,
        APPROVED);
    when(requisitionTemplate.isColumnInTemplateAndDisplayed(APPROVED_QUANTITY)).thenReturn(false);

    // when
    List<Requisition> result = requisitionService.releaseWithoutOrder(requisitions);
    assertEquals(result.size(), requisitions.size());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotReleaseRequisitionsWithoutOrderIfInIncorrectStatus() {
    // given
    List<ReleasableRequisitionDto> requisitions = setUpReleaseRequisitionsAsOrder(1,
        SUBMITTED);

    // when
    requisitionService.releaseWithoutOrder(requisitions);
  }

  private void validateRequisitionDeleteWithStatus(RequisitionStatus status) {
    requisition.setStatus(status);
    when(statusMessageRepository.findByRequisitionId(requisition.getId()))
        .thenReturn(Collections.emptyList());
    stubRecentRequisition();

    requisitionService.delete(requisition);
    verify(requisitionRepository).delete(requisition);
  }

  private List<FacilityDto> mockSupplyingDepot(UUID supplyingDepotId) {
    FacilityDto supplyingDepot = new FacilityDto();
    supplyingDepot.setId(supplyingDepotId);
    return new ArrayList<>(Arrays.asList(supplyingDepot));
  }

  private Pageable mockPageable() {
    Pageable pageable = mock(Pageable.class);
    Sort sort = new Sort(Sort.Direction.DESC, "programName");
    when(pageable.getSort()).thenReturn(sort);
    return pageable;
  }

  private List<ReleasableRequisitionDto> setUpReleaseRequisitionsAsOrder(
      int amount, RequisitionStatus status) {
    if (amount < 1) {
      throw new IllegalArgumentException("Amount must be a positive number");
    }

    List<ReleasableRequisitionDto> result = new ArrayList<>();

    for (int i = 0; i < amount; i++) {
      FacilityDto facility = mock(FacilityDto.class);
      when(facility.getId()).thenReturn(UUID.randomUUID());

      Requisition requisition = new Requisition(UUID.randomUUID(), UUID.randomUUID(),
          UUID.randomUUID(), status, false);
      requisition.setId(UUID.randomUUID());
      requisition.setSupervisoryNodeId(UUID.randomUUID());
      requisition.setSupplyingFacilityId(facility.getId());
      requisition.setRequisitionLineItems(Lists.newArrayList());
      requisition.setTemplate(requisitionTemplate);

      when(requisitionRepository.findOne(requisition.getId())).thenReturn(requisition);
      when(facilityReferenceDataService.findOne(facility.getId())).thenReturn(facility);
      when(facilityReferenceDataService
          .searchSupplyingDepots(requisition.getProgramId(), requisition.getSupervisoryNodeId()))
          .thenReturn(singletonList(facility));

      result.add(new ReleasableRequisitionDto(requisition.getId(), facility.getId()));
    }

    return result;
  }

  private Requisition generateRequisition() {
    requisition = new Requisition(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(),
        INITIATED, false);
    requisition.setId(UUID.randomUUID());
    requisition.setCreatedDate(ZonedDateTime.now());
    requisition.setSupplyingFacilityId(facility.getId());
    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(lineItem1);
    requisitionLineItems.add(lineItem2);
    requisition.setRequisitionLineItems(requisitionLineItems);
    requisition.setTemplate(requisitionTemplate);
    requisition.setFacilityId(facility.getId());
    requisition.setProgramId(program.getId());
    requisition.setSupervisoryNodeId(supervisoryNode.getId());
    requisition.setStatus(AUTHORIZED);
    return requisition;
  }

  private RequisitionDto generateRequisitionDto() {
    requisitionDto = new RequisitionDto();
    requisitionDto.setId(requisition.getId());
    requisitionDto.setSupervisoryNode(supervisoryNode.getId());
    requisitionDto.setStatus(AUTHORIZED);
    return requisitionDto;
  }

  private void generateReasons() {
    StockAdjustmentReason reason = new StockAdjustmentReason();
    reason.setId(UUID.randomUUID());
    reason.setReasonCategory(ReasonCategory.ADJUSTMENT);
    reason.setReasonType(ReasonType.DEBIT);
    reason.setDescription("simple description");
    reason.setIsFreeTextAllowed(false);
    reason.setName("simple name");
    stockAdjustmentReasons = singletonList(reason);
  }

  private List<BasicRequisitionDto> getBasicRequisitionDtoList() {
    List<BasicRequisitionDto> requisitionDtos = new ArrayList<>();
    String[] programNames = {"one", "two", "three", "four", "five"};

    for (String programName : programNames) {
      BasicRequisitionDto requisitionDto = new BasicRequisitionDto();
      ProgramDto programDto = new ProgramDto();
      programDto.setName(programName);
      requisitionDto.setProgram(programDto);
      requisitionDtos.add(requisitionDto);
    }
    return requisitionDtos;
  }

  private void setupStubsForTestFindSupplyItems(
      Requisition requisition, List<RequisitionLineItem> fullSupply,
      List<RequisitionLineItem> nonFullSupply) {
    OrderableDto fullSupplyOrderable = new OrderableDtoDataBuilder()
        .withProgramOrderable(requisition.getProgramId(), true)
        .build();
    when(orderableReferenceDataService.findOne(fullSupplyOrderable.getId()))
        .thenReturn(fullSupplyOrderable);

    OrderableDto nonFullSupplyOrderable = new OrderableDtoDataBuilder()
        .withProgramOrderable(requisition.getProgramId(), false)
        .build();
    when(orderableReferenceDataService.findOne(nonFullSupplyOrderable.getId()))
        .thenReturn(nonFullSupplyOrderable);

    fullSupply.forEach(line -> when(line.getOrderableId())
        .thenReturn(fullSupplyOrderable.getId()));
    nonFullSupply.forEach(line -> when(line.getOrderableId())
        .thenReturn(nonFullSupplyOrderable.getId()));
  }

  private void setupStubsForTestApprovedRequisition(List<BasicRequisitionDto> requisitionDtos,
                                                    String filterBy, String programName,
                                                    String facilityCode, String facilityName,
                                                    List<FacilityDto> supplyingDepots,
                                                    Pageable pageable, int pageSize,
                                                    int pageNumber) {
    final List<UUID> programs = new ArrayList<>();
    final List<UUID> facilitys = new ArrayList<>();
    final List<Requisition> requisitions = new ArrayList<>();

    when(programReferenceDataService.search(programName))
        .thenReturn(Collections.emptyList());
    when(programReferenceDataService.findAll())
        .thenReturn(Collections.emptyList());
    when(facilityReferenceDataService.search(eq(facilityCode), eq(facilityName),
        eq(null), eq(false))).thenReturn(Collections.emptyList());
    when(facilityReferenceDataService.findAll())
        .thenReturn(Collections.emptyList());
    when(requisitionRepository.searchApprovedRequisitions(filterBy, programs, facilitys))
        .thenReturn(requisitions);

    when(requisitionRepository.findOne(any(UUID.class))).thenReturn(mock(Requisition.class));

    List<RequisitionWithSupplyingDepotsDto> requisitionsWithDepots = new ArrayList<>();
    for (BasicRequisitionDto dto : requisitionDtos) {
      requisitionsWithDepots.add(new RequisitionWithSupplyingDepotsDto(dto, supplyingDepots));
    }
    when(requisitionForConvertBuilder.buildRequisitions(any(), any(), any(), any()))
        .thenReturn(requisitionsWithDepots);

    when(pageable.getPageSize()).thenReturn(pageSize);
    when(pageable.getPageNumber()).thenReturn(pageNumber);
  }

  private void mockPreviousRequisition() {
    RequisitionLineItem previousRequisitionLineItem = new RequisitionLineItem();
    previousRequisitionLineItem.setAdjustedConsumption(ADJUSTED_CONSUMPTION);
    previousRequisitionLineItem.setOrderableId(PRODUCT_ID);
    previousRequisition = new Requisition();
    previousRequisition.setId(UUID.randomUUID());
    previousRequisition
        .setRequisitionLineItems(singletonList(previousRequisitionLineItem));

    when(requisitionRepository
        .searchRequisitions(any(), eq(facility.getId()), eq(program.getId()), eq(false)))
        .thenReturn(singletonList(previousRequisition));
  }

  private void mockNoPreviousRequisition() {
    when(requisitionRepository
        .searchRequisitions(any(), any(), any(), any()))
        .thenReturn(Collections.emptyList());
  }

  private void mockApprovedProduct(UUID[] products, boolean[] fullSupply) {
    assertThat(products.length, is(fullSupply.length));

    List<ApprovedProductDto> approvedProducts = new ArrayList<>();

    for (int i = 0, length = products.length; i < length; ++i) {
      approvedProducts.add(new ApprovedProductDtoDataBuilder()
          .withOrderable(new OrderableDtoDataBuilder()
              .withId(products[i])
              .withFullProductName(productNamePrefix + i)
              .withIdentifier(COMMODITY_TYPE, COMMODITY_TYPE_ID.toString())
              .withProgramOrderable(program.getId(), fullSupply[i])
              .build())
          .withProgram(program)
          .build()
      );
    }

    when(approvedProductReferenceDataService.getApprovedProducts(any(), any()))
        .thenReturn(new ApproveProductsAggregator(approvedProducts, program.getId()));
  }

  private void prepareForTestInitiate(Integer numberOfPeriodsToAverage) {
    when(requisitionTemplate.getNumberOfPeriodsToAverage()).thenReturn(numberOfPeriodsToAverage);

    when(requisitionRepository.findOne(requisition.getId())).thenReturn(null);

    when(facilityReferenceDataService.findOne(facility.getId()))
        .thenReturn(new FacilityDto());
    when(programReferenceDataService.findOne(program.getId()))
        .thenReturn(new ProgramDto());

    when(periodService.findPeriod(
        eq(program.getId()), eq(facility.getId()), eq(processingPeriod.getId()), anyBoolean()))
        .thenReturn(processingPeriod);
  }

  private List<Requisition> mockSearchRequisitionsForApproval() {
    requisition.setStatus(IN_APPROVAL);
    List<Requisition> requisitions = new ArrayList<>();
    requisitions.add(requisition);
    Requisition requisition2 = generateRequisition();
    requisition2.setStatus(AUTHORIZED);
    requisitions.add(requisition2);

    when(requisitionRepository.searchApprovableRequisitionsByProgramSupervisoryNodePairs(
        newHashSet(new ImmutablePair<>(program.getId(), supervisoryNode.getId())), pageRequest))
        .thenReturn(getPage(requisitions, pageRequest));
    return requisitions;
  }

  private void mockRepositories() {
    when(requisitionRepository
        .findOne(requisition.getId()))
        .thenReturn(requisition);
    when(requisitionRepository
        .save(requisition))
        .thenReturn(requisition);
    when(requisitionRepository
        .save(requisition))
        .thenReturn(requisition);
    when(programReferenceDataService
        .findOne(any()))
        .thenReturn(program);
    when(authenticationHelper
        .getRight(PermissionService.ORDERS_EDIT))
        .thenReturn(convertToOrderRight);
    when(rightReferenceDataService
        .findRight(PermissionService.REQUISITION_APPROVE))
        .thenReturn(approveRequisitionRight);

    processingPeriod.setDurationInMonths(1);
    when(periodService
        .getPeriod(any()))
        .thenReturn(processingPeriod);

    when(periodService
        .findPeriod(program.getId(), facility.getId(), processingPeriod.getId(), false))
        .thenReturn(processingPeriod);

    when(scheduleReferenceDataService.searchByProgramAndFacility(any(), any()))
        .thenReturn(Arrays.asList(processingPeriod.getProcessingSchedule()));

    when(periodService.searchByProgramAndFacility(any(), any()))
        .thenReturn(Arrays.asList(processingPeriod));

    when(requisitionRepository.searchRequisitions(any(), any(), any(), any()))
        .thenReturn(new ArrayList<>());

    when(idealStockAmountReferenceDataService.search(facility.getId(), processingPeriod.getId()))
        .thenReturn(Lists.newArrayList());

    when(orderDtoBuilder.build(any(Requisition.class), any(UserDto.class)))
        .thenAnswer(new Answer<OrderDto>() {
          @Override
          public OrderDto answer(InvocationOnMock invocation) throws Throwable {
            Requisition requisition = (Requisition) invocation.getArguments()[0];

            if (null == requisition) {
              return null;
            }

            OrderDto order = new OrderDto();
            order.setExternalId(requisition.getId());
            order.setEmergency(requisition.getEmergency());
            order.setQuotedCost(BigDecimal.ZERO);

            order.setOrderLineItems(
                requisition
                    .getRequisitionLineItems()
                    .stream()
                    .map(line -> OrderLineItemDto.newOrderLineItem(line, null))
                    .collect(Collectors.toList())
            );

            order.setCreatedBy((UserDto) invocation.getArguments()[1]);

            return order;
          }
        });

    UUID currentUserId = UUID.randomUUID();
    UserDto currentUser = new UserDto();
    currentUser.setId(currentUserId);
    when(authenticationHelper.getCurrentUser()).thenReturn(currentUser);
  }

  private void stubRecentRequisition() {
    when(periodService.searchByProgramAndFacility(program.getId(), facility.getId()))
        .thenReturn(singleton(processingPeriod));
    when(requisitionRepository
        .searchRequisitions(processingPeriod.getId(), facility.getId(), program.getId(), false))
        .thenReturn(singletonList(requisition));
  }

  private void stubPreviousPeriod() {
    ProcessingPeriodDto periodDto = new ProcessingPeriodDto();
    periodDto.setId(PERIOD_ID);
    when(periodService.findPreviousPeriods(any(), eq(SETTING - 1)))
        .thenReturn(singletonList(periodDto));
  }

  private void prepareRequisitionIsNotNewest() {
    when(periodService.searchByProgramAndFacility(program.getId(), facility.getId()))
        .thenReturn(singleton(processingPeriod));
    Requisition newestRequisition = mock(Requisition.class);
    when(newestRequisition.getId()).thenReturn(UUID.randomUUID());
    when(requisitionRepository
        .searchRequisitions(processingPeriod.getId(), facility.getId(), program.getId(), false))
        .thenReturn(singletonList(newestRequisition));
  }

  private OngoingStubbing<List<StockCardSummaryDto>> whenGetStockCardSummaries() {
    return when(stockCardSummariesStockManagementService
        .search(any(UUID.class), any(UUID.class), anySetOf(UUID.class),
            any(LocalDate.class)));
  }

  private OngoingStubbing<List<StockCardRangeSummaryDto>> whenGetStockCardRangeSummaries() {
    return when(stockCardRangeSummaryStockManagementService
        .search(any(UUID.class), any(UUID.class), anySetOf(UUID.class), any(String.class),
            any(LocalDate.class), any(LocalDate.class)));
  }

  private void prepareForGetStockOnHandTest() {
    prepareForTestInitiate(SETTING);
    when(requisitionTemplate.isPopulateStockOnHandFromStockCards()).thenReturn(true);
    whenGetStockCardRangeSummaries().thenReturn(singletonList(stockCardRangeSummaryDto));
    when(stockCardSummariesStockManagementService
        .search(program.getId(), facility.getId(), singleton(PRODUCT_ID),
            processingPeriod.getEndDate()))
        .thenReturn(singletonList(stockCardSummaryDto));
    ReflectionTestUtils.setField(stockCardSummaryDto.getOrderable(), "id", PRODUCT_ID);
  }

}
