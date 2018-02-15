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
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.RequisitionLineItem.APPROVED_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionStatus.APPROVED;
import static org.openlmis.requisition.domain.RequisitionStatus.AUTHORIZED;
import static org.openlmis.requisition.domain.RequisitionStatus.INITIATED;
import static org.openlmis.requisition.domain.RequisitionStatus.IN_APPROVAL;
import static org.openlmis.requisition.domain.RequisitionStatus.REJECTED;
import static org.openlmis.requisition.domain.RequisitionStatus.RELEASED;
import static org.openlmis.requisition.domain.RequisitionStatus.SKIPPED;
import static org.openlmis.requisition.domain.RequisitionStatus.SUBMITTED;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.runners.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;
import org.mockito.stubbing.OngoingStubbing;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.StatusChange;
import org.openlmis.requisition.domain.StatusMessage;
import org.openlmis.requisition.domain.StockAdjustmentReason;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.BasicRequisitionDto;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.DetailedRoleAssignmentDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.dto.OrderLineItemDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ReasonCategory;
import org.openlmis.requisition.dto.ReasonType;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.RoleDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.SupplyLineDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.dto.stockmanagement.StockCardSummaryDto;
import org.openlmis.requisition.errorhandling.FailureType;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.openlmis.requisition.service.fulfillment.OrderFulfillmentService;
import org.openlmis.requisition.service.referencedata.ApprovedProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.IdealStockAmountReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.RightReferenceDataService;
import org.openlmis.requisition.service.referencedata.ScheduleReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserRoleAssignmentsReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.StockCardSummariesStockManagementService;
import org.openlmis.requisition.settings.service.ConfigurationSettingService;
import org.openlmis.requisition.testutils.ApprovedProductDtoDataBuilder;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.testutils.IdealStockAmountDtoDataBuilder;
import org.openlmis.requisition.testutils.OrderableDtoDataBuilder;
import org.openlmis.requisition.testutils.SupplyLineDtoDataBuilder;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.openlmis.requisition.utils.Pagination;
import org.openlmis.requisition.utils.RightName;
import org.openlmis.requisition.web.BasicRequisitionDtoBuilder;
import org.openlmis.requisition.web.OrderDtoBuilder;
import org.openlmis.requisition.web.RequisitionForConvertBuilder;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.util.ReflectionTestUtils;

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
  private RequisitionTemplate requisitionTemplate;

  @Mock
  private ProgramDto program;

  @Mock
  private FacilityDto facility;

  @Mock
  private SupervisoryNodeDto supervisoryNode;

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
  private RequisitionTemplateRepository requisitionTemplateRepository;

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
  UserRoleAssignmentsReferenceDataService userRoleAssignmentsReferenceDataService;

  @Mock
  private UserReferenceDataService userReferenceDataService;

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

  @InjectMocks
  private RequisitionService requisitionService;

  @Mock
  private BasicRequisitionDtoBuilder basicRequisitionDtoBuilder;

  private RightDto convertToOrderRight = DtoGenerator.of(RightDto.class, 2).get(0);
  private RightDto approveRequisitionRight = DtoGenerator.of(RightDto.class, 2).get(1);
  private RoleDto role = DtoGenerator.of(RoleDto.class);
  private UserDto user = DtoGenerator.of(UserDto.class);
  private StockCardSummaryDto stockCard = DtoGenerator.of(StockCardSummaryDto.class);

  private static final int SETTING = 5;
  private static final int ADJUSTED_CONSUMPTION = 7;
  private static final UUID PRODUCT_ID = UUID.randomUUID();
  private static final UUID NON_FULL_PRODUCT_ID = UUID.randomUUID();

  private ProcessingPeriodDto processingPeriodDto;
  private List<StockAdjustmentReason> stockAdjustmentReasons;
  private UUID programId = UUID.randomUUID();
  private UUID facilityId = UUID.randomUUID();
  private UUID suggestedPeriodId = UUID.randomUUID();
  private UUID supervisoryNodeId = UUID.randomUUID();
  private UUID userId = UUID.randomUUID();
  private List<String> permissionStrings = singletonList("validPermissionString");
  private PageRequest pageRequest = new PageRequest(
      Pagination.DEFAULT_PAGE_NUMBER, Pagination.NO_PAGINATION);
  Requisition previousRequisition;
  private LocalDate periodEndDate = LocalDate.of(2017, 12, 30);

  @Before
  public void setUp() {
    generateRequisition();
    generateRequisitionDto();
    generateReasons();
    mockRepositories();
    when(permissionService.getPermissionStrings()).thenReturn(permissionStrings);
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

    requisitionService.delete(requisition.getId());
    verify(requisitionRepository).delete(requisition);
  }

  @Test
  public void shouldDeleteRequisitionIfRecentRequisitionIsNotInLastPeriod() {
    requisition.setStatus(INITIATED);

    ProcessingPeriodDto secondPeriod = mock(ProcessingPeriodDto.class);
    when(secondPeriod.getId()).thenReturn(UUID.randomUUID());
    when(periodService.searchByProgramAndFacility(programId, facilityId))
        .thenReturn(Arrays.asList(processingPeriodDto, secondPeriod));
    when(requisitionRepository
        .searchRequisitions(processingPeriodDto.getId(), facilityId, programId, false))
        .thenReturn(singletonList(requisition));
    when(requisitionRepository
        .searchRequisitions(secondPeriod.getId(), facilityId, programId, false))
        .thenReturn(Collections.emptyList());

    requisitionService.delete(requisition.getId());
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

    requisitionService.delete(requisition.getId());
    verify(requisitionRepository).delete(requisition);
    verify(statusMessageRepository).delete(statusMessages);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotDeleteRequisitionWhenStatusIsAuthorized() throws
      ValidationMessageException {
    requisition.setStatus(AUTHORIZED);
    stubRecentRequisition();

    requisitionService.delete(requisition.getId());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotDeleteRequisitionWhenStatusIsInApproval() throws
      ValidationMessageException {
    requisition.setStatus(IN_APPROVAL);
    stubRecentRequisition();

    requisitionService.delete(requisition.getId());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotDeleteRequisitionWhenStatusIsApproved() throws
      ValidationMessageException {
    requisition.setStatus(APPROVED);
    stubRecentRequisition();

    requisitionService.delete(requisition.getId());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotDeleteRequisitionIfItIsNotNewest() {
    requisition.setStatus(INITIATED);

    prepareRequisitionIsNotNewest();

    requisitionService.delete(requisition.getId());
  }

  @Test
  public void shouldDeleteEmergencyRequisitionEvenIfItIsNewest() {
    requisition.setStatus(INITIATED);
    requisition.setEmergency(true);
    prepareRequisitionIsNotNewest();

    requisitionService.delete(requisition.getId());
    verify(requisitionRepository).delete(requisition);
  }

  @Test(expected = ContentNotFoundMessageException.class)
  public void shouldThrowExceptionWhenDeletingNotExistingRequisition()
      throws ContentNotFoundMessageException {
    UUID deletedRequisitionId = requisition.getId();
    when(requisitionRepository
        .findOne(requisition.getId()))
        .thenReturn(null);
    requisitionService.delete(deletedRequisitionId);
  }

  @Test
  public void shouldSkipRequisitionIfItIsValid() {
    when(program.getPeriodsSkippable()).thenReturn(true);
    requisition.setStatus(INITIATED);
    Requisition skippedRequisition = requisitionService.skip(requisition.getId());
    verify(lineItem1).skipLineItem(requisition.getTemplate());
    verify(lineItem2).skipLineItem(requisition.getTemplate());

    assertEquals(skippedRequisition.getStatus(), SKIPPED);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenSkippingNotSkippableProgram() {
    when(program.getPeriodsSkippable()).thenReturn(false);
    requisitionService.skip(requisition.getId());
  }


  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenSkippingSubmittedRequisition() {
    when(program.getPeriodsSkippable()).thenReturn(true);
    requisition.setStatus(SUBMITTED);
    requisitionService.skip(requisition.getId());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenSkippingAuthorizedRequisition() {
    when(program.getPeriodsSkippable()).thenReturn(true);
    requisition.setStatus(AUTHORIZED);
    requisitionService.skip(requisition.getId());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenSkippingInApprovalRequisition() {
    when(program.getPeriodsSkippable()).thenReturn(true);
    requisition.setStatus(IN_APPROVAL);
    requisitionService.skip(requisition.getId());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenSkippingApprovedRequisition() {
    when(program.getPeriodsSkippable()).thenReturn(true);
    requisition.setStatus(APPROVED);
    requisitionService.skip(requisition.getId());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenSkippingReleasedRequisition() {
    when(program.getPeriodsSkippable()).thenReturn(true);
    requisition.setStatus(RELEASED);
    requisitionService.skip(requisition.getId());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenSkippingEmergencyRequisition() {
    when(program.getPeriodsSkippable()).thenReturn(true);
    requisition.setEmergency(true);
    requisitionService.skip(requisition.getId());
  }

  @Test(expected = ContentNotFoundMessageException.class)
  public void shouldThrowExceptionWhenSkippingNotExistingRequisition()
      throws ContentNotFoundMessageException {
    when(requisitionRepository
        .findOne(requisition.getId()))
        .thenReturn(null);
    requisitionService.skip(requisition.getId());
  }

  @Test
  public void shouldRejectRequisitionIfRequisitionStatusIsAuthorized() {
    requisition.setStatus(AUTHORIZED);
    when(permissionService.canApproveRequisition(any(UUID.class)))
        .thenReturn(ValidationResult.success());
    when(userRoleAssignmentsReferenceDataService.hasSupervisionRight(any(RightDto.class),
        any(UUID.class), any(UUID.class), any(UUID.class)))
        .thenReturn(true);
    Requisition returnedRequisition = requisitionService.reject(requisition.getId());

    assertEquals(returnedRequisition.getStatus(), REJECTED);
  }

  @Test
  public void shouldRejectRequisitionIfRequisitionStatusIsInApproval() {
    requisition.setStatus(IN_APPROVAL);
    when(permissionService.canApproveRequisition(any(UUID.class)))
        .thenReturn(ValidationResult.success());
    when(userRoleAssignmentsReferenceDataService.hasSupervisionRight(any(RightDto.class),
        any(UUID.class), any(UUID.class), any(UUID.class)))
        .thenReturn(true);
    Requisition returnedRequisition = requisitionService.reject(requisition.getId());

    assertEquals(returnedRequisition.getStatus(), REJECTED);
  }

  @Test
  public void shouldSaveStatusMessageWhileRejectingRequisition() {
    requisition.setStatus(AUTHORIZED);
    requisition.setDraftStatusMessage("some_message");
    when(permissionService.canApproveRequisition(any(UUID.class)))
        .thenReturn(ValidationResult.success());
    when(userRoleAssignmentsReferenceDataService.hasSupervisionRight(any(RightDto.class),
        any(UUID.class), any(UUID.class), any(UUID.class)))
        .thenReturn(true);
    Requisition returnedRequisition = requisitionService.reject(requisition.getId());

    assertEquals(returnedRequisition.getStatus(), REJECTED);
    verify(statusMessageRepository, times(1)).save(any(StatusMessage.class));
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenRejectingRequisitionWithStatusSubmitted()
      throws ValidationMessageException {
    requisition.setStatus(SUBMITTED);
    requisitionService.reject(requisition.getId());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenRejectingRequisitionWithStatusApproved()
      throws ValidationMessageException {
    requisition.setStatus(APPROVED);
    requisitionService.reject(requisition.getId());
  }

  @Test(expected = ContentNotFoundMessageException.class)
  public void shouldThrowExceptionWhenRejectingNotExistingRequisition()
      throws ContentNotFoundMessageException {
    when(requisitionRepository.findOne(requisition.getId())).thenReturn(null);
    requisitionService.reject(requisition.getId());
  }

  @Test
  public void shouldGetApprovableRequisitionsWhenStatusIsAuthorized() {
    requisition.setStatus(AUTHORIZED);

    when(requisitionRepository.searchRequisitions(
        null, programId, null, null, null, supervisoryNodeId, null, null, permissionStrings,
        pageRequest))
        .thenReturn(Pagination.getPage(singletonList(requisition), pageRequest));

    List<Requisition> authorizedRequisitions =
        requisitionService.getApprovableRequisitions(program.getId(), supervisoryNode.getId());
    List<Requisition> expected = singletonList(requisition);

    assertEquals(expected, authorizedRequisitions);
  }

  @Test
  public void shouldGetApprovableRequisitionsWhenStatusIsInApproval() {
    requisition.setStatus(IN_APPROVAL);

    when(requisitionRepository.searchRequisitions(
        null, programId, null, null, null, supervisoryNodeId, null, null, permissionStrings,
        pageRequest))
        .thenReturn(Pagination.getPage(singletonList(requisition), pageRequest));

    List<Requisition> inApprovalRequisitions =
        requisitionService.getApprovableRequisitions(program.getId(), supervisoryNode.getId());
    List<Requisition> expected = singletonList(requisition);

    assertEquals(expected, inApprovalRequisitions);
  }

  @Test
  public void shouldGetRequisitionsForApproval() {
    // given
    List<Requisition> requisitions = mockSearchRequisitionsForApproval();
    assertEquals(2, requisitions.size());

    DetailedRoleAssignmentDto detailedRoleAssignmentDto = mock(DetailedRoleAssignmentDto.class);
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(programId);
    when(detailedRoleAssignmentDto.getSupervisoryNodeId()).thenReturn(supervisoryNodeId);
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
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(programId);
    when(detailedRoleAssignmentDto.getSupervisoryNodeId()).thenReturn(supervisoryNodeId);
    when(detailedRoleAssignmentDto.getRole()).thenReturn(role);

    Set<RightDto> rights = new HashSet<>();
    rights.add(approveRequisitionRight);
    role.setRights(rights);

    Set<DetailedRoleAssignmentDto> roleAssignmentDtos = new HashSet<>();
    roleAssignmentDtos.add(detailedRoleAssignmentDto);
    when(userRoleAssignmentsReferenceDataService.getRoleAssignments(user.getId()))
            .thenReturn(roleAssignmentDtos);

    Page<Requisition> requisitionsForApproval =
            requisitionService.getRequisitionsForApproval(user.getId(), programId, pageRequest);

    assertEquals(2, requisitionsForApproval.getTotalElements());
    assertTrue(requisitionsForApproval.getContent().contains(requisitions.get(0)));
    assertTrue(requisitionsForApproval.getContent().contains(requisitions.get(1)));
  }

  @Test
  public void shouldNotGetRequisitionsForApprovalWithoutApproveRight() {
    DetailedRoleAssignmentDto detailedRoleAssignmentDto = mock(DetailedRoleAssignmentDto.class);
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(programId);
    when(detailedRoleAssignmentDto.getSupervisoryNodeId()).thenReturn(supervisoryNodeId);
    when(detailedRoleAssignmentDto.getRole()).thenReturn(role);

    Set<RightDto> rights = new HashSet<>();
    role.setRights(rights);

    Set<DetailedRoleAssignmentDto> roleAssignmentDtos = new HashSet<>();
    roleAssignmentDtos.add(detailedRoleAssignmentDto);
    when(userRoleAssignmentsReferenceDataService.getRoleAssignments(user.getId()))
        .thenReturn(roleAssignmentDtos);
    when(requisitionRepository.searchApprovableRequisitionsByProgramSupervisoryNodePairs(
        any(Set.class), any(Pageable.class)))
        .thenReturn(Pagination.getPage(Collections.emptyList(), pageRequest));

    Page<Requisition> requisitionsForApproval =
        requisitionService.getRequisitionsForApproval(user.getId(), null, pageRequest);

    assertEquals(0, requisitionsForApproval.getTotalElements());
  }

  @Test
  public void shouldNotGetRequisitionsForApprovalWithIncorrectSupervisoryNode() {
    DetailedRoleAssignmentDto detailedRoleAssignmentDto = mock(DetailedRoleAssignmentDto.class);
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(programId);
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
        .thenReturn(Pagination.getPage(Collections.emptyList(), pageRequest));

    Page<Requisition> requisitionsForApproval =
        requisitionService.getRequisitionsForApproval(user.getId(), null, pageRequest);

    assertEquals(0, requisitionsForApproval.getTotalElements());
  }

  @Test
  public void shouldNotGetRequisitionsForApprovalWithIncorrectProgram() {
    DetailedRoleAssignmentDto detailedRoleAssignmentDto = mock(DetailedRoleAssignmentDto.class);
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(UUID.randomUUID());
    when(detailedRoleAssignmentDto.getSupervisoryNodeId()).thenReturn(supervisoryNodeId);
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
        .thenReturn(Pagination.getPage(Collections.emptyList(), pageRequest));

    Page<Requisition> requisitionsForApproval =
        requisitionService.getRequisitionsForApproval(user.getId(), null, pageRequest);

    assertEquals(0, requisitionsForApproval.getTotalElements());
  }

  @Test
  public void shouldPassValidationIfUserCanApproveRequisition() {
    when(permissionService.canApproveRequisition(any(UUID.class)))
        .thenReturn(ValidationResult.success());
    when(userRoleAssignmentsReferenceDataService.hasSupervisionRight(any(RightDto.class),
        any(UUID.class), any(UUID.class), any(UUID.class)))
        .thenReturn(true);

    ValidationResult result = requisitionService.validateCanApproveRequisition(
        requisition, requisition.getId(), userId);

    assertTrue(result.isSuccess());
  }

  @Test
  public void shouldFailValidationIfUserHasNoApproveRightAssigned() {
    when(permissionService.canApproveRequisition(any(UUID.class)))
        .thenReturn(ValidationResult.noPermission("no.permission"));

    ValidationResult result = requisitionService.validateCanApproveRequisition(requisition,
        requisition.getId(), userId);

    assertTrue(result.hasErrors());
    assertEquals(FailureType.NO_PERMISSION, result.getError().getType());
  }

  @Test
  public void shouldFailValidationIfRequisitionDoesNotExist() {
    when(permissionService.canApproveRequisition(any(UUID.class)))
        .thenReturn(ValidationResult.success());

    ValidationResult result = requisitionService.validateCanApproveRequisition(null,
        UUID.randomUUID(), userId);

    assertTrue(result.hasErrors());
    assertEquals(FailureType.NOT_FOUND, result.getError().getType());
  }

  @Test
  public void shouldFailValidationIfRequisitionIsInIncorrectState() {
    when(permissionService.canApproveRequisition(any(UUID.class)))
        .thenReturn(ValidationResult.success());
    requisition.setStatus(INITIATED);

    ValidationResult result = requisitionService.validateCanApproveRequisition(requisition,
        UUID.randomUUID(), userId);

    assertTrue(result.hasErrors());
    assertEquals(FailureType.VALIDATION, result.getError().getType());
  }

  @Test
  public void shouldFailValidationIfUserHasNoUpdateRightsAssigned() {
    when(permissionService.canUpdateRequisition(requisitionDto.getId()))
        .thenReturn(ValidationResult.noPermission("no.permission"));

    ValidationResult result = requisitionService.validateCanSaveRequisition(requisitionDto.getId());

    assertTrue(result.hasErrors());
    assertEquals(FailureType.NO_PERMISSION, result.getError().getType());
  }

  @Test
  public void shouldFailValidationIfRequisitionDoesNotExistOnSave() {
    when(permissionService.canUpdateRequisition(any(UUID.class)))
        .thenReturn(ValidationResult.success());
    when(requisitionRepository.findOne(requisitionDto.getId())).thenReturn(null);

    ValidationResult result = requisitionService.validateCanSaveRequisition(requisitionDto.getId());

    assertTrue(result.hasErrors());
    assertEquals(FailureType.NOT_FOUND, result.getError().getType());
  }

  @Test
  public void shouldFailValidationIfRequisitionIsInIncorrectStatus() {
    when(permissionService.canUpdateRequisition(any(UUID.class)))
        .thenReturn(ValidationResult.success());
    when(requisitionRepository.findOne(requisitionDto.getId())).thenReturn(requisition);
    requisition.setStatus(RELEASED);

    ValidationResult result = requisitionService.validateCanSaveRequisition(requisitionDto.getId());

    assertTrue(result.hasErrors());
    assertEquals(FailureType.VALIDATION, result.getError().getType());
  }

  @Test
  public void shouldPassValidationIfUserCanUpdateRequisition() {
    when(permissionService.canUpdateRequisition(any(UUID.class)))
        .thenReturn(ValidationResult.success());
    when(requisitionRepository.findOne(requisitionDto.getId())).thenReturn(requisition);
    requisition.setStatus(SUBMITTED);

    ValidationResult result = requisitionService.validateCanSaveRequisition(requisitionDto.getId());

    assertTrue(result.isSuccess());
  }

  @Test
  public void shouldInitiateRequisitionIfItDoesNotAlreadyExist() {
    prepareForTestInitiate(SETTING);
    RequisitionTemplate requisitionTemplate = mock(RequisitionTemplate.class);
    when(requisitionTemplate.hasColumnsDefined()).thenReturn(true);
    when(requisitionTemplate.getNumberOfPeriodsToAverage()).thenReturn(2);

    when(requisitionRepository
        .findOne(requisition.getId()))
        .thenReturn(null);

    doReturn(requisitionTemplate).when(requisitionTemplateRepository)
        .getTemplateForProgram(programId);

    ProcessingPeriodDto periodDto = new ProcessingPeriodDto();
    periodDto.setStartDate(LocalDate.of(2016, 11, 1));
    periodDto.setEndDate(LocalDate.of(2016, 11, 30));
    periodDto.setDurationInMonths(1);
    doReturn(periodDto).when(periodService).findPeriod(programId, facilityId, suggestedPeriodId,
        false);

    Requisition initiatedRequisition = requisitionService.initiate(
        programId, facilityId, suggestedPeriodId, false,
        stockAdjustmentReasons);

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
        programId, facilityId, suggestedPeriodId, false,
        stockAdjustmentReasons);

    RequisitionLineItem requisitionLineItem = initiatedRequisition.getRequisitionLineItems().get(0);
    assertEquals(Integer.valueOf(ADJUSTED_CONSUMPTION),
        requisitionLineItem.getPreviousAdjustedConsumptions().get(0));
    verify(requisitionRepository).searchRequisitions(
            initiatedRequisition.getProcessingPeriodId(), facilityId, programId, false);
  }

  @Test
  public void shouldInitiatePreviousAdjustedConsumptionsBasedOnRegularRequisitions() {
    prepareForTestInitiate(SETTING);
    stubPreviousPeriod();
    mockPreviousRequisition();
    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        programId, facilityId, suggestedPeriodId, false,
        stockAdjustmentReasons);

    RequisitionLineItem requisitionLineItem = initiatedRequisition.getRequisitionLineItems().get(0);
    assertEquals(Integer.valueOf(ADJUSTED_CONSUMPTION),
        requisitionLineItem.getPreviousAdjustedConsumptions().get(0));
    verify(requisitionRepository).searchRequisitions(
        PERIOD_ID, facilityId, programId, false);
  }

  @Test
  public void shouldAssignPreviousRegularRequisition() {
    prepareForTestInitiate(SETTING);
    stubPreviousPeriod();
    mockPreviousRequisition();
    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        programId, facilityId, suggestedPeriodId, false,
        stockAdjustmentReasons);

    UUID previousRequisitionId = initiatedRequisition.getPreviousRequisitions().get(0).getId();
    assertEquals(previousRequisition.getId(), previousRequisitionId);
    verify(requisitionRepository).searchRequisitions(
        PERIOD_ID, facilityId, programId, false);
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
        programId, facilityId, suggestedPeriodId, false,
        stockAdjustmentReasons);

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
        this.programId, facilityId, suggestedPeriodId, false,
        stockAdjustmentReasons);

    verify(periodService, times(2)).findPreviousPeriods(any(), eq(1));
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
        this.programId, facilityId, suggestedPeriodId, false,
        stockAdjustmentReasons);

    RequisitionLineItem requisitionLineItem = initiatedRequisition.getRequisitionLineItems().get(0);
    assertEquals(0, requisitionLineItem.getPreviousAdjustedConsumptions().size());
  }

  @Test
  public void shouldNotSetPreviousAdjustedConsumptionsIfNoRequisitionForNoPreviousPeriod() {
    prepareForTestInitiate(SETTING);
    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        this.programId, facilityId, suggestedPeriodId, false,
        stockAdjustmentReasons);

    RequisitionLineItem requisitionLineItem = initiatedRequisition.getRequisitionLineItems().get(0);
    assertEquals(0, requisitionLineItem.getPreviousAdjustedConsumptions().size());
  }

  @Test
  public void shouldSetStockAdjustmenReasonsDuringInitiate() {
    prepareForTestInitiate(SETTING);

    Requisition initiatedRequisition = requisitionService.initiate(
        programId, facilityId, suggestedPeriodId, false,
        stockAdjustmentReasons);

    assertEquals(stockAdjustmentReasons, initiatedRequisition.getStockAdjustmentReasons());
  }

  @Test
  public void shouldPopulateOnlyNonFullProductsDuringInitiateForRegularRequisition() {
    prepareForTestInitiate(SETTING);
    mockApprovedProduct(new UUID[]{PRODUCT_ID, NON_FULL_PRODUCT_ID}, new boolean[]{true, false});

    Requisition initiatedRequisition = requisitionService.initiate(
        this.programId, facilityId, suggestedPeriodId, false,
        stockAdjustmentReasons);

    Set<UUID> availableProducts = initiatedRequisition.getAvailableProducts();
    assertThat(availableProducts, hasSize(1));
    assertThat(availableProducts, hasItems(NON_FULL_PRODUCT_ID));
  }

  @Test
  public void shouldPopulateFullAndNonFullProductsDuringInitiateForEmergencyRequisition() {
    prepareForTestInitiate(SETTING);
    mockApprovedProduct(new UUID[]{PRODUCT_ID, NON_FULL_PRODUCT_ID}, new boolean[]{true, false});

    Requisition initiatedRequisition = requisitionService.initiate(
        this.programId, facilityId, suggestedPeriodId, true,
        stockAdjustmentReasons);

    Set<UUID> availableProducts = initiatedRequisition.getAvailableProducts();
    assertThat(availableProducts, hasSize(2));
    assertThat(availableProducts, hasItems(PRODUCT_ID, NON_FULL_PRODUCT_ID));
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenInitiatingEmptyRequisition() {
    requisitionService.initiate(null, null, null, false, stockAdjustmentReasons);
  }

  @Test
  public void shouldSetStockOnHandFromStockIfFlagIsEnabled() {
    prepareForGetStockOnHandTest();

    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        programId, facilityId, suggestedPeriodId, false, stockAdjustmentReasons);

    assertEquals(stockCard.getStockOnHand(),
        initiatedRequisition.getRequisitionLineItems().get(0).getStockOnHand());
  }

  @Test
  public void shouldSetNullStockOnHandFromStockIfFlagIsEnabled() {
    prepareForGetStockOnHandTest();
    ReflectionTestUtils.setField(stockCard, "stockOnHand", null);

    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        programId, facilityId, suggestedPeriodId, false, stockAdjustmentReasons);

    assertNull(initiatedRequisition.getRequisitionLineItems().get(0).getStockOnHand());
  }

  @Test
  public void shouldNotSetStockOnHandIfFlagIsDisabled() {
    prepareForTestInitiate(SETTING, requisitionTemplate);
    when(requisitionTemplate.isPopulateStockOnHandFromStockCards()).thenReturn(false);
    whenGetStockCardSummaries().thenThrow(IllegalStateException.class);

    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        programId, facilityId, suggestedPeriodId, false, stockAdjustmentReasons);

    assertNull(initiatedRequisition.getRequisitionLineItems().get(0).getStockOnHand());
  }

  @Test
  public void shouldNotSetStockOnHandIfNoStockCardSummariesFound() {
    prepareForTestInitiate(SETTING, requisitionTemplate);
    when(requisitionTemplate.isPopulateStockOnHandFromStockCards()).thenReturn(true);
    whenGetStockCardSummaries().thenReturn(Collections.emptyList());

    mockApprovedProduct(new UUID[]{PRODUCT_ID}, new boolean[]{true});

    Requisition initiatedRequisition = requisitionService.initiate(
        programId, facilityId, suggestedPeriodId, false, stockAdjustmentReasons);

    assertNull(initiatedRequisition.getRequisitionLineItems().get(0).getStockOnHand());
  }

  @Test
  public void shouldReleaseRequisitionsAsOrder() {
    // given
    List<ConvertToOrderDto> requisitions = setUpReleaseRequisitionsAsOrder(5, APPROVED);
    List<FacilityDto> facilities = requisitions.stream()
        .map(r -> facilityReferenceDataService.findOne(r.getSupplyingDepotId()))
        .collect(Collectors.toList());

    when(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(user.getId(),
        convertToOrderRight.getId())).thenReturn(facilities);
    when(requisitionForConvertBuilder.getAvailableSupplyingDepots(any(UUID.class)))
        .thenReturn(facilities);

    // when
    List<Requisition> expectedRequisitions = requisitionService
        .releaseRequisitionsAsOrder(requisitions, user);

    // then
    for (Requisition requisition : expectedRequisitions) {
      assertEquals(RELEASED, requisition.getStatus());
    }
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotReleaseRequisitionsAsOrderIfSupplyingDepotsNotProvided() {
    // given
    List<ConvertToOrderDto> requisitions = setUpReleaseRequisitionsAsOrder(5, APPROVED);
    List<FacilityDto> facilities = requisitions.stream()
        .map(r -> facilityReferenceDataService.findOne(r.getSupplyingDepotId()))
        .collect(Collectors.toList());

    when(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(user.getId(),
        convertToOrderRight.getId())).thenReturn(facilities);

    for (ConvertToOrderDto requisition : requisitions) {
      requisition.setSupplyingDepotId(null);
    }

    // when
    requisitionService.releaseRequisitionsAsOrder(requisitions, user);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotReleaseRequisitionsAsOrderIfUserHasNoFulfillmentRightsForFacility() {
    // given
    List<ConvertToOrderDto> requisitions = setUpReleaseRequisitionsAsOrder(5, APPROVED);

    when(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(user.getId(),
        convertToOrderRight.getId())).thenReturn(new ArrayList<>());

    // when
    requisitionService.releaseRequisitionsAsOrder(requisitions, user);
  }


  @Test(expected = ValidationMessageException.class)
  public void shouldNotReleaseRequisitionsAsOrderIfApprovedQtyDisabled() {
    // given
    List<ConvertToOrderDto> requisitions = setUpReleaseRequisitionsAsOrder(1, APPROVED);
    when(requisitionTemplate.isColumnInTemplateAndDisplayed(APPROVED_QUANTITY)).thenReturn(false);

    // when
    requisitionService.releaseRequisitionsAsOrder(requisitions, user);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotReleaseRequisitionsAsOrderInIncorrectStatus() {
    // given
    List<ConvertToOrderDto> requisitions = setUpReleaseRequisitionsAsOrder(1, SUBMITTED);

    // when
    requisitionService.releaseRequisitionsAsOrder(requisitions, user);
  }

  @Test
  public void shouldCallApproveRequisition() {
    OrderableDto fullSupplyOrderable = new OrderableDtoDataBuilder().build();
    SupplyLineDto supplyLineDto = new SupplyLineDtoDataBuilder().build();
    when(orderableReferenceDataService.findByIds(any())).thenReturn(
        singletonList(fullSupplyOrderable));

    UUID parentId = UUID.randomUUID();
    UUID userId = UUID.randomUUID();
    Set<UUID> orderableIds = singleton(UUID.randomUUID());

    requisitionService.doApprove(
        parentId, userId, orderableIds, requisitionMock, singletonList(supplyLineDto)
    );

    verify(requisitionMock, times(1)).approve(eq(parentId),
        eq(singletonList(fullSupplyOrderable)),
        eq(singletonList(supplyLineDto)), eq(userId));
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
        .thenReturn(Pagination.getPage(singletonList(requisition), pageRequest));

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
    //when
    requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(
        Lists.newArrayList(emProgram, fpProgram), filterBy, pageable,
        Arrays.asList(supplyingDepotId));

    // then
    verify(programReferenceDataService).search(emProgram);
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

    List<ConvertToOrderDto> list = setUpReleaseRequisitionsAsOrder(requisitionsCount, APPROVED);

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

    List<ConvertToOrderDto> list = setUpReleaseRequisitionsAsOrder(requisitionsCount, APPROVED);

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
    List<ConvertToOrderDto> list = setUpReleaseRequisitionsAsOrder(1, APPROVED);

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
  public void shouldReturnFullSupplyRequisitionLineItems() {
    // given
    Requisition requisition = generateRequisition();
    when(requisitionRepository.findOne(requisition.getId())).thenReturn(requisition);

    List<RequisitionLineItem> fullSupply = singletonList(lineItem1);
    List<RequisitionLineItem> nonFullSupply = singletonList(lineItem2);

    setupStubsForTestFindSupplyItems(requisition, fullSupply, nonFullSupply);

    // when
    List<RequisitionLineItem> result = requisitionService.getFullSupplyItems(requisition.getId());

    Set<RequisitionLineItem> resultSet = new HashSet<>(result);
    Set<RequisitionLineItem> fullSupplySet = new HashSet<>(fullSupply);

    // then
    assertTrue(resultSet.equals(fullSupplySet));
  }

  @Test
  public void shouldReturnNonFullSupplyRequisitionLineItems() {
    // given
    Requisition requisition = generateRequisition();
    when(requisitionRepository.findOne(requisition.getId())).thenReturn(requisition);

    List<RequisitionLineItem> fullSupply = singletonList(lineItem1);
    List<RequisitionLineItem> nonFullSupply = singletonList(lineItem2);

    setupStubsForTestFindSupplyItems(requisition, fullSupply, nonFullSupply);

    // when
    List<RequisitionLineItem> result = requisitionService
        .getNonFullSupplyItems(requisition.getId());

    Set<RequisitionLineItem> resultSet = new HashSet<>(result);
    Set<RequisitionLineItem> nonFullSupplySet = new HashSet<>(nonFullSupply);

    // then
    assertTrue(resultSet.equals(nonFullSupplySet));
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
    requisitionService.saveStatusMessage(requisition);

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
    requisitionService.saveStatusMessage(requisition);

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
    requisitionService.saveStatusMessage(requisition);

    requisition.setDraftStatusMessage("");
    requisitionService.saveStatusMessage(requisition);

    // then
    verify(statusMessageRepository, never()).save(any(StatusMessage.class));
  }

  private void validateRequisitionDeleteWithStatus(RequisitionStatus status) {
    requisition.setStatus(status);
    when(statusMessageRepository.findByRequisitionId(requisition.getId()))
        .thenReturn(Collections.emptyList());
    stubRecentRequisition();

    requisitionService.delete(requisition.getId());
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

  private List<ConvertToOrderDto> setUpReleaseRequisitionsAsOrder(
      int amount, RequisitionStatus status) {
    if (amount < 1) {
      throw new IllegalArgumentException("Amount must be a positive number");
    }

    List<ConvertToOrderDto> result = new ArrayList<>();

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

      result.add(new ConvertToOrderDto(requisition.getId(), facility.getId()));
    }

    return result;
  }

  private Requisition generateRequisition() {
    requisition = new Requisition(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(),
        INITIATED, false);
    requisition.setId(UUID.randomUUID());
    requisition.setCreatedDate(ZonedDateTime.now());
    requisition.setSupplyingFacilityId(facilityId);
    List<RequisitionLineItem> requisitionLineItems = new ArrayList<>();
    requisitionLineItems.add(lineItem1);
    requisitionLineItems.add(lineItem2);
    requisition.setRequisitionLineItems(requisitionLineItems);
    requisition.setTemplate(requisitionTemplate);
    requisition.setFacilityId(facilityId);
    requisition.setProgramId(programId);
    requisition.setSupervisoryNodeId(supervisoryNodeId);
    requisition.setStatus(AUTHORIZED);
    return requisition;
  }

  private RequisitionDto generateRequisitionDto() {
    requisitionDto = new RequisitionDto();
    requisitionDto.setId(UUID.randomUUID());
    requisitionDto.setSupervisoryNode(supervisoryNodeId);
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
    final List<UUID> programIds = new ArrayList<>();
    final List<UUID> facilityIds = new ArrayList<>();
    final List<Requisition> requisitions = new ArrayList<>();

    when(programReferenceDataService.search(programName))
        .thenReturn(Collections.emptyList());
    when(programReferenceDataService.findAll())
        .thenReturn(Collections.emptyList());
    when(facilityReferenceDataService.search(eq(facilityCode), eq(facilityName),
        eq(null), eq(false))).thenReturn(Collections.emptyList());
    when(facilityReferenceDataService.findAll())
        .thenReturn(Collections.emptyList());
    when(requisitionRepository.searchApprovedRequisitions(filterBy, programIds, facilityIds))
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
        .searchRequisitions(any(), eq(facilityId), eq(programId), eq(false)))
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
              .withIdentifier(COMMODITY_TYPE, COMMODITY_TYPE_ID.toString())
              .withProgramOrderable(programId, fullSupply[i])
              .build())
          .withProgram(program)
          .build()
      );
    }

    when(approvedProductReferenceDataService.getApprovedProducts(any(), any()))
        .thenReturn(approvedProducts);
  }

  private void prepareForTestInitiate(Integer numberOfPeriodsToAverage) {
    RequisitionTemplate requisitionTemplate = mock(RequisitionTemplate.class);
    prepareForTestInitiate(numberOfPeriodsToAverage, requisitionTemplate);
  }

  private void prepareForTestInitiate(Integer numberOfPeriodsToAverage,
                                      RequisitionTemplate requisitionTemplate) {
    when(requisitionTemplate.hasColumnsDefined()).thenReturn(true);
    when(requisitionTemplate.getNumberOfPeriodsToAverage()).thenReturn(numberOfPeriodsToAverage);

    when(requisitionRepository.findOne(requisition.getId())).thenReturn(null);

    when(facilityReferenceDataService.findOne(facilityId)).thenReturn(mock(FacilityDto.class));
    when(programReferenceDataService.findOne(programId)).thenReturn(mock(ProgramDto.class));
    when(requisitionTemplateRepository.getTemplateForProgram(programId))
        .thenReturn(requisitionTemplate);

    ProcessingPeriodDto periodDto = new ProcessingPeriodDto();
    periodDto.setDurationInMonths(1);
    periodDto.setEndDate(periodEndDate);
    when(periodService
        .findPeriod(eq(programId), eq(facilityId), eq(suggestedPeriodId), anyBoolean()))
        .thenReturn(periodDto);
  }

  private List<Requisition> mockSearchRequisitionsForApproval() {
    requisition.setStatus(IN_APPROVAL);
    List<Requisition> requisitions = new ArrayList<>();
    requisitions.add(requisition);
    Requisition requisition2 = generateRequisition();
    requisition2.setStatus(AUTHORIZED);
    requisitions.add(requisition2);

    when(requisitionRepository.searchApprovableRequisitionsByProgramSupervisoryNodePairs(
        Sets.newHashSet(new ImmutablePair<>(programId, supervisoryNodeId)), pageRequest))
        .thenReturn(Pagination.getPage(requisitions, pageRequest));
    return requisitions;
  }

  private void mockRepositories() {
    ProcessingScheduleDto processingScheduleDto = new ProcessingScheduleDto();
    processingScheduleDto.setId(UUID.randomUUID());

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
        .getRight(RightName.ORDERS_EDIT))
        .thenReturn(convertToOrderRight);
    when(rightReferenceDataService
        .findRight(RightName.REQUISITION_APPROVE))
        .thenReturn(approveRequisitionRight);

    when(facility.getId()).thenReturn(facilityId);
    when(program.getId()).thenReturn(programId);
    when(supervisoryNode.getId()).thenReturn(supervisoryNodeId);
    when(requisitionTemplate.isColumnInTemplateAndDisplayed(APPROVED_QUANTITY)).thenReturn(true);

    processingPeriodDto = new ProcessingPeriodDto();
    processingPeriodDto.setProcessingSchedule(processingScheduleDto);
    processingPeriodDto.setId(suggestedPeriodId);
    processingPeriodDto.setDurationInMonths(1);
    when(periodService
        .getPeriod(any()))
        .thenReturn(processingPeriodDto);

    when(periodService
        .findPeriod(programId, facilityId, suggestedPeriodId, false))
        .thenReturn(processingPeriodDto);

    when(scheduleReferenceDataService.searchByProgramAndFacility(any(), any()))
        .thenReturn(Arrays.asList(processingScheduleDto));

    when(periodService.searchByProgramAndFacility(any(), any()))
        .thenReturn(Arrays.asList(processingPeriodDto));

    when(requisitionRepository.searchRequisitions(any(), any(), any(), any()))
        .thenReturn(new ArrayList<>());

    when(idealStockAmountReferenceDataService.search(facilityId, processingPeriodDto.getId()))
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
    when(periodService.searchByProgramAndFacility(programId, facilityId))
        .thenReturn(singleton(processingPeriodDto));
    when(requisitionRepository
        .searchRequisitions(processingPeriodDto.getId(), facilityId, programId, false))
        .thenReturn(singletonList(requisition));
  }

  private void stubPreviousPeriod() {
    ProcessingPeriodDto periodDto = new ProcessingPeriodDto();
    periodDto.setId(PERIOD_ID);
    when(periodService.findPreviousPeriods(any(), eq(SETTING - 1)))
        .thenReturn(singletonList(periodDto));
  }

  private void prepareRequisitionIsNotNewest() {
    when(periodService.searchByProgramAndFacility(programId, facilityId))
        .thenReturn(singleton(processingPeriodDto));
    Requisition newestRequisition = mock(Requisition.class);
    when(newestRequisition.getId()).thenReturn(UUID.randomUUID());
    when(requisitionRepository
        .searchRequisitions(processingPeriodDto.getId(), facilityId, programId, false))
        .thenReturn(singletonList(newestRequisition));
  }

  private OngoingStubbing<List<StockCardSummaryDto>> whenGetStockCardSummaries() {
    return when(stockCardSummariesStockManagementService
        .search(any(UUID.class), any(UUID.class), anySetOf(UUID.class),
            any(LocalDate.class)));
  }

  private void prepareForGetStockOnHandTest() {
    prepareForTestInitiate(SETTING, requisitionTemplate);
    when(requisitionTemplate.isPopulateStockOnHandFromStockCards()).thenReturn(true);
    when(stockCardSummariesStockManagementService
        .search(programId, facilityId, singleton(PRODUCT_ID), periodEndDate))
        .thenReturn(singletonList(stockCard));
    ReflectionTestUtils.setField(stockCard.getOrderable(), "id", PRODUCT_ID);
  }

}
