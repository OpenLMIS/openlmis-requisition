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

import static org.hamcrest.Matchers.hasItem;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.RequisitionLineItem.AVERAGE_CONSUMPTION;
import static org.openlmis.requisition.domain.RequisitionLineItem.BEGINNING_BALANCE;
import static org.openlmis.requisition.domain.RequisitionStatus.APPROVED;
import static org.openlmis.requisition.domain.RequisitionStatus.AUTHORIZED;
import static org.openlmis.requisition.domain.RequisitionStatus.INITIATED;
import static org.openlmis.requisition.domain.RequisitionStatus.IN_APPROVAL;
import static org.openlmis.requisition.domain.RequisitionStatus.RELEASED;
import static org.openlmis.requisition.domain.RequisitionStatus.SKIPPED;
import static org.openlmis.requisition.domain.RequisitionStatus.SUBMITTED;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

import org.joda.money.CurrencyUnit;
import org.joda.money.Money;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.runners.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.StatusMessage;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.ConvertToOrderDto;
import org.openlmis.requisition.dto.DetailedRoleAssignmentDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.dto.OrderLineItemDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.dto.RightDto;
import org.openlmis.requisition.dto.RoleDto;
import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.dto.SupplyLineDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.openlmis.requisition.service.fulfillment.OrderFulfillmentService;
import org.openlmis.requisition.service.referencedata.ApprovedProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.service.referencedata.RightReferenceDataService;
import org.openlmis.requisition.service.referencedata.ScheduleReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupplyLineReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserFulfillmentFacilitiesReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserRoleAssignmentsReferenceDataService;
import org.openlmis.requisition.web.OrderDtoBuilder;
import org.openlmis.settings.exception.ConfigurationSettingException;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.AuthenticationHelper;
import org.openlmis.utils.ConvertHelper;
import org.openlmis.utils.RequisitionDtoComparator;
import org.openlmis.utils.RightName;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnusedPrivateField"})
@RunWith(MockitoJUnitRunner.class)
public class RequisitionServiceTest {

  private Requisition requisition;

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
  private RightDto convertToOrderRight;

  @Mock
  private RightDto approveRequisitionRight;

  @Mock
  private RoleDto role;

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
  private RequisitionTemplateService requisitionTemplateService;

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
  private SupplyLineReferenceDataService supplyLineService;

  @Mock
  private OrderFulfillmentService orderFulfillmentService;

  @Mock
  private ConvertHelper convertHelper;

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

  @InjectMocks
  private RequisitionService requisitionService;

  private static final int SETTING = 5;
  private static final int ADJUSTED_CONSUMPTION = 7;
  private static final UUID PRODUCT_ID = UUID.randomUUID();
  private static final UUID NON_FULL_PRODUCT_ID = UUID.randomUUID();

  private ProcessingPeriodDto processingPeriodDto;
  private UUID programId = UUID.randomUUID();
  private UUID facilityId = UUID.randomUUID();
  private UUID suggestedPeriodId = UUID.randomUUID();
  private UUID convertToOrderRightId = UUID.randomUUID();
  private UUID approveRequisitionRightId = UUID.randomUUID();
  private UUID supervisoryNodeId = UUID.randomUUID();
  private UUID userId = UUID.randomUUID();

  @Before
  public void setUp() {
    generateRequisition();
    mockRepositories();
  }

  @Test
  public void shouldDeleteRequisitionIfItIsInitiated() {
    requisition.setStatus(INITIATED);
    when(statusMessageRepository.findByRequisitionId(requisition.getId()))
        .thenReturn(Collections.emptyList());
    requisitionService.delete(requisition.getId());
    verify(requisitionRepository).delete(requisition);
  }

  @Test
  public void shouldDeleteRequisitionWhenStatusIsSubmitted() {
    requisition.setStatus(SUBMITTED);
    when(statusMessageRepository.findByRequisitionId(requisition.getId()))
            .thenReturn(Collections.emptyList());
    requisitionService.delete(requisition.getId());
    verify(requisitionRepository).delete(requisition);
  }

  @Test
  public void shouldDeleteStatusMessagesWhenDeletingRequisition() {
    requisition.setStatus(INITIATED);
    List<StatusMessage> statusMessages = Collections.singletonList(
        StatusMessage.newStatusMessage(requisition, null, null, null, "Message 1"));
    when(statusMessageRepository.findByRequisitionId(requisition.getId()))
        .thenReturn(statusMessages);
    requisitionService.delete(requisition.getId());
    verify(requisitionRepository).delete(requisition);
    verify(statusMessageRepository).delete(statusMessages);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotDeleteRequisitionWhenStatusIsAuthorized() throws
      ValidationMessageException {
    requisition.setStatus(AUTHORIZED);
    requisitionService.delete(requisition.getId());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotDeleteRequisitionWhenStatusIsInApproval() throws
      ValidationMessageException {
    requisition.setStatus(IN_APPROVAL);
    requisitionService.delete(requisition.getId());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotDeleteRequisitionWhenStatusIsApproved() throws
      ValidationMessageException {
    requisition.setStatus(APPROVED);
    requisitionService.delete(requisition.getId());
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
    Requisition returnedRequisition = requisitionService.reject(requisition.getId());

    assertEquals(returnedRequisition.getStatus(), INITIATED);
  }

  @Test
  public void shouldRejectRequisitionIfRequisitionStatusIsInApproval() {
    requisition.setStatus(IN_APPROVAL);
    Requisition returnedRequisition = requisitionService.reject(requisition.getId());

    assertEquals(returnedRequisition.getStatus(), INITIATED);
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
        null, programId, null, null, null, supervisoryNodeId, null, null))
        .thenReturn(Collections.singletonList(requisition));

    List<Requisition> authorizedRequisitions =
        requisitionService.getApprovableRequisitions(program.getId(), supervisoryNode.getId());
    List<Requisition> expected = Collections.singletonList(requisition);

    assertEquals(expected, authorizedRequisitions);
  }

  @Test
  public void shouldGetApprovableRequisitionsWhenStatusIsInApproval() {
    requisition.setStatus(IN_APPROVAL);

    when(requisitionRepository.searchRequisitions(
        null, programId, null, null, null, supervisoryNodeId, null, null))
        .thenReturn(Collections.singletonList(requisition));

    List<Requisition> inApprovalRequisitions =
        requisitionService.getApprovableRequisitions(program.getId(), supervisoryNode.getId());
    List<Requisition> expected = Collections.singletonList(requisition);

    assertEquals(expected, inApprovalRequisitions);
  }

  @Test
  public void shouldGetRequisitionsForApproval() {
    List<Requisition> requisitions = mockSearchRequisitionsForApproval();
    assertEquals(2, requisitions.size());

    DetailedRoleAssignmentDto detailedRoleAssignmentDto = mock(DetailedRoleAssignmentDto.class);
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(programId);
    when(detailedRoleAssignmentDto.getSupervisoryNodeId()).thenReturn(supervisoryNodeId);
    when(detailedRoleAssignmentDto.getRole()).thenReturn(role);

    Set<RightDto> rights = new HashSet<>();
    rights.add(approveRequisitionRight);
    when(role.getRights()).thenReturn(rights);

    Set<DetailedRoleAssignmentDto> roleAssignmentDtos = new HashSet<>();
    roleAssignmentDtos.add(detailedRoleAssignmentDto);
    UUID userId = UUID.randomUUID();
    UserDto user = mock(UserDto.class);
    when(user.getId()).thenReturn(userId);
    when(userRoleAssignmentsReferenceDataService.getRoleAssignments(userId))
        .thenReturn(roleAssignmentDtos);

    when(requisitionRepository.searchRequisitions(
        requisition.getFacilityId(),
        requisition.getProgramId(),
        requisition.getCreatedDate().minusDays(2),
        requisition.getCreatedDate().plusDays(2),
        requisition.getProcessingPeriodId(),
        requisition.getSupervisoryNodeId(),
        EnumSet.of(requisition.getStatus()), null))
        .thenReturn(requisitions);

    Set<Requisition> requisitionsForApproval =
        requisitionService.getRequisitionsForApproval(userId, null);

    assertEquals(2, requisitionsForApproval.size());
    assertTrue(requisitionsForApproval.contains(requisitions.get(0)));
    assertTrue(requisitionsForApproval.contains(requisitions.get(1)));
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
    when(role.getRights()).thenReturn(rights);

    Set<DetailedRoleAssignmentDto> roleAssignmentDtos = new HashSet<>();
    roleAssignmentDtos.add(detailedRoleAssignmentDto);
    UUID userId = UUID.randomUUID();
    UserDto user = mock(UserDto.class);
    when(user.getId()).thenReturn(userId);
    when(userRoleAssignmentsReferenceDataService.getRoleAssignments(userId))
            .thenReturn(roleAssignmentDtos);

    when(requisitionRepository.searchRequisitions(
            requisition.getFacilityId(),
            requisition.getProgramId(),
            requisition.getCreatedDate().minusDays(2),
            requisition.getCreatedDate().plusDays(2),
            requisition.getProcessingPeriodId(),
            requisition.getSupervisoryNodeId(),
            EnumSet.of(requisition.getStatus()), null))
            .thenReturn(requisitions);

    Set<Requisition> requisitionsForApproval =
            requisitionService.getRequisitionsForApproval(userId, programId);

    assertEquals(2, requisitionsForApproval.size());
    assertTrue(requisitionsForApproval.contains(requisitions.get(0)));
    assertTrue(requisitionsForApproval.contains(requisitions.get(1)));

    requisitionsForApproval =
            requisitionService.getRequisitionsForApproval(userId, UUID.randomUUID());

    assertEquals(0, requisitionsForApproval.size());
  }

  @Test
  public void shouldntGetRequisitionsForApprovalWithoutApproveRight() {
    mockSearchRequisitionsForApproval();

    DetailedRoleAssignmentDto detailedRoleAssignmentDto = mock(DetailedRoleAssignmentDto.class);
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(programId);
    when(detailedRoleAssignmentDto.getSupervisoryNodeId()).thenReturn(supervisoryNodeId);
    when(detailedRoleAssignmentDto.getRole()).thenReturn(role);

    Set<RightDto> rights = new HashSet<>();
    when(role.getRights()).thenReturn(rights);

    Set<DetailedRoleAssignmentDto> roleAssignmentDtos = new HashSet<>();
    roleAssignmentDtos.add(detailedRoleAssignmentDto);
    UUID userId = UUID.randomUUID();
    UserDto user = mock(UserDto.class);
    when(user.getId()).thenReturn(userId);
    when(userRoleAssignmentsReferenceDataService.getRoleAssignments(userId))
        .thenReturn(roleAssignmentDtos);

    Set<Requisition> requisitionsForApproval =
        requisitionService.getRequisitionsForApproval(userId, null);

    assertEquals(0, requisitionsForApproval.size());
  }

  @Test
  public void shouldntGetRequisitionsForApprovalWithIncorrectSupervisoryNode() {
    mockSearchRequisitionsForApproval();

    DetailedRoleAssignmentDto detailedRoleAssignmentDto = mock(DetailedRoleAssignmentDto.class);
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(programId);
    when(detailedRoleAssignmentDto.getSupervisoryNodeId()).thenReturn(UUID.randomUUID());
    when(detailedRoleAssignmentDto.getRole()).thenReturn(role);

    Set<RightDto> rights = new HashSet<>();
    rights.add(approveRequisitionRight);
    when(role.getRights()).thenReturn(rights);

    Set<DetailedRoleAssignmentDto> roleAssignmentDtos = new HashSet<>();
    roleAssignmentDtos.add(detailedRoleAssignmentDto);
    UUID userId = UUID.randomUUID();
    UserDto user = mock(UserDto.class);
    when(user.getId()).thenReturn(userId);
    when(userRoleAssignmentsReferenceDataService.getRoleAssignments(userId))
        .thenReturn(roleAssignmentDtos);

    Set<Requisition> requisitionsForApproval =
        requisitionService.getRequisitionsForApproval(userId, null);

    assertEquals(0, requisitionsForApproval.size());
  }

  @Test
  public void shouldntGetRequisitionsForApprovalWithIncorrectProgram() {
    mockSearchRequisitionsForApproval();

    DetailedRoleAssignmentDto detailedRoleAssignmentDto = mock(DetailedRoleAssignmentDto.class);
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(UUID.randomUUID());
    when(detailedRoleAssignmentDto.getSupervisoryNodeId()).thenReturn(supervisoryNodeId);
    when(detailedRoleAssignmentDto.getRole()).thenReturn(role);

    Set<RightDto> rights = new HashSet<>();
    rights.add(approveRequisitionRight);
    when(role.getRights()).thenReturn(rights);

    Set<DetailedRoleAssignmentDto> roleAssignmentDtos = new HashSet<>();
    roleAssignmentDtos.add(detailedRoleAssignmentDto);
    UUID userId = UUID.randomUUID();
    UserDto user = mock(UserDto.class);
    when(user.getId()).thenReturn(userId);
    when(userRoleAssignmentsReferenceDataService.getRoleAssignments(userId))
        .thenReturn(roleAssignmentDtos);

    Set<Requisition> requisitionsForApproval =
        requisitionService.getRequisitionsForApproval(userId, null);

    assertEquals(0, requisitionsForApproval.size());
  }

  @Test
  public void shouldReturnTrueIfUserCanApproveRequisition() {
    mockSupervisionRoleAssignmentForCanApprove(programId, supervisoryNodeId,
        approveRequisitionRight);

    assertTrue(requisitionService.canApproveRequisition(programId, supervisoryNodeId, userId));
  }

  @Test
  public void shouldReturnFalseIfUserHasNoApproveRigthAssigned() {
    mockSupervisionRoleAssignmentForCanApprove(programId, supervisoryNodeId, convertToOrderRight);

    assertFalse(requisitionService.canApproveRequisition(programId, supervisoryNodeId, userId));
  }

  @Test
  public void shouldReturnFalseIfUserHasDifferentSupervisoryNodeAssignedToApproveRight() {
    mockSupervisionRoleAssignmentForCanApprove(programId, UUID.randomUUID(),
        approveRequisitionRight);

    assertFalse(requisitionService.canApproveRequisition(programId, supervisoryNodeId, userId));
  }

  @Test
  public void shouldReturnFalseIfUserHasDifferentProgramAssignedToApproveRight() {
    mockSupervisionRoleAssignmentForCanApprove(UUID.randomUUID(), supervisoryNodeId,
        approveRequisitionRight);

    assertFalse(requisitionService.canApproveRequisition(programId, supervisoryNodeId, userId));
  }

  @Test
  public void shouldReturnFalseIfUserWasNotPassedAsArgument() {
    assertFalse(requisitionService.canApproveRequisition(programId, supervisoryNodeId, null));
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

    doReturn(requisitionTemplate).when(requisitionTemplateService).getTemplateForProgram(programId);

    ProcessingPeriodDto periodDto = new ProcessingPeriodDto();
    periodDto.setStartDate(LocalDate.of(2016, 11, 1));
    periodDto.setEndDate(LocalDate.of(2016, 11, 30));
    periodDto.setDurationInMonths(1);
    doReturn(periodDto).when(periodService).findPeriod(programId, facilityId, suggestedPeriodId,
        false);

    Requisition initiatedRequisition = requisitionService.initiate(
        programId, facilityId, suggestedPeriodId, false
    );

    assertEquals(INITIATED, initiatedRequisition.getStatus());
    assertEquals(1, initiatedRequisition.getNumberOfMonthsInPeriod().longValue());
  }

  @Test
  public void shouldInitiatePreviousAdjustedConsumptions() {
    prepareForTestInitiate(SETTING);
    when(periodService.findPreviousPeriods(any(), eq(SETTING - 1)))
        .thenReturn(Collections.singletonList(new ProcessingPeriodDto()));
    mockPreviousRequisition();
    mockApprovedProduct(PRODUCT_ID, true);

    Requisition initiatedRequisition = requisitionService.initiate(
        this.programId, facilityId, suggestedPeriodId, false
    );

    RequisitionLineItem requisitionLineItem = initiatedRequisition.getRequisitionLineItems().get(0);
    assertEquals(Integer.valueOf(ADJUSTED_CONSUMPTION),
        requisitionLineItem.getPreviousAdjustedConsumptions().get(0));
  }

  @Test
  public void shouldSetEmptyPreviousAdjustedConsumptionsWhenNumberOfPeriodsToAverageIsNull() {
    prepareForTestInitiate(null);
    mockPreviousRequisition();
    mockApprovedProduct(PRODUCT_ID, true);

    Requisition initiatedRequisition = requisitionService.initiate(
        this.programId, facilityId, suggestedPeriodId, false
    );

    verify(periodService, times(2)).findPreviousPeriods(any(), eq(1));
    RequisitionLineItem requisitionLineItem = initiatedRequisition.getRequisitionLineItems().get(0);
    assertEquals(0, requisitionLineItem.getPreviousAdjustedConsumptions().size());
  }

  @Test
  public void shouldNotSetPreviousAdjustedConsumptionsIfNoRequisitionForNoPreviousRequisition() {
    prepareForTestInitiate(SETTING);
    when(periodService.findPreviousPeriods(any(), eq(SETTING - 1)))
        .thenReturn(Collections.singletonList(new ProcessingPeriodDto()));
    mockNoPreviousRequisition();
    mockApprovedProduct(PRODUCT_ID, true);

    Requisition initiatedRequisition = requisitionService.initiate(
        this.programId, facilityId, suggestedPeriodId, false
    );

    RequisitionLineItem requisitionLineItem = initiatedRequisition.getRequisitionLineItems().get(0);
    assertEquals(0, requisitionLineItem.getPreviousAdjustedConsumptions().size());
  }

  @Test
  public void shouldNotSetPreviousAdjustedConsumptionsIfNoRequisitionForNoPreviousPeriod() {
    prepareForTestInitiate(SETTING);
    mockApprovedProduct(PRODUCT_ID, true);

    Requisition initiatedRequisition = requisitionService.initiate(
        this.programId, facilityId, suggestedPeriodId, false
    );

    RequisitionLineItem requisitionLineItem = initiatedRequisition.getRequisitionLineItems().get(0);
    assertEquals(0, requisitionLineItem.getPreviousAdjustedConsumptions().size());
  }

  @Test
  public void shouldSetAvailableNonFullSupplyProductsForRequisition() {
    prepareForTestInitiate(SETTING);
    mockApprovedProduct(PRODUCT_ID, true);
    mockApprovedProduct(NON_FULL_PRODUCT_ID, false);

    Requisition initiatedRequisition = requisitionService.initiate(
        this.programId, facilityId, suggestedPeriodId, false
    );

    Set<UUID> availableNonFullSupplyProducts = initiatedRequisition
        .getAvailableNonFullSupplyProducts();
    assertThat(availableNonFullSupplyProducts, hasItem(NON_FULL_PRODUCT_ID));
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenInitiatingEmptyRequisition() {
    requisitionService.initiate(null, null, null, false);
  }

  @Test
  public void shouldReleaseRequisitionsAsOrder() {
    // given
    UserDto user = mock(UserDto.class);
    UUID userId = UUID.randomUUID();
    when(user.getId()).thenReturn(userId);

    List<ConvertToOrderDto> requisitions = setUpReleaseRequisitionsAsOrder(5);
    List<FacilityDto> facilities = requisitions.stream()
        .map(r -> facilityReferenceDataService.findOne(r.getSupplyingDepotId()))
        .collect(Collectors.toList());

    when(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(userId,
        convertToOrderRightId)).thenReturn(facilities);

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
    UserDto user = mock(UserDto.class);
    UUID userId = UUID.randomUUID();
    when(user.getId()).thenReturn(userId);

    List<ConvertToOrderDto> requisitions = setUpReleaseRequisitionsAsOrder(5);
    List<FacilityDto> facilities = requisitions.stream()
        .map(r -> facilityReferenceDataService.findOne(r.getSupplyingDepotId()))
        .collect(Collectors.toList());

    when(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(userId,
        convertToOrderRightId)).thenReturn(facilities);

    for (ConvertToOrderDto requisition : requisitions) {
      requisition.setSupplyingDepotId(null);
    }

    // when
    requisitionService.releaseRequisitionsAsOrder(requisitions, user);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotReleaseRequisitionsAsOrderIfUserHasNoFulfillmentRightsForFacility() {
    // given
    UserDto user = mock(UserDto.class);
    UUID userId = UUID.randomUUID();
    when(user.getId()).thenReturn(userId);

    List<ConvertToOrderDto> requisitions = setUpReleaseRequisitionsAsOrder(5);

    when(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(userId,
        convertToOrderRightId)).thenReturn(new ArrayList<>());

    // when
    requisitionService.releaseRequisitionsAsOrder(requisitions, user);
  }

  @Test
  public void shouldFindRequisitionIfItExists() {

    when(requisitionRepository.searchRequisitions(
        requisition.getFacilityId(),
        requisition.getProgramId(),
        requisition.getCreatedDate().minusDays(2),
        requisition.getCreatedDate().plusDays(2),
        requisition.getProcessingPeriodId(),
        requisition.getSupervisoryNodeId(),
        EnumSet.of(requisition.getStatus()), null))
        .thenReturn(Collections.singletonList(requisition));

    List<Requisition> receivedRequisitions = requisitionService.searchRequisitions(
        requisition.getFacilityId(),
        requisition.getProgramId(),
        requisition.getCreatedDate().minusDays(2),
        requisition.getCreatedDate().plusDays(2),
        requisition.getProcessingPeriodId(),
        requisition.getSupervisoryNodeId(),
        EnumSet.of(requisition.getStatus()), null);

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
  public void shouldFindAndSortAndPageApprovedRequisitions() {
    // given
    List<RequisitionDto> requisitionDtos = getRequisitionDtoList();

    String filterAndSortBy = "programName";

    UUID supplyingDepotId = UUID.randomUUID();
    FacilityDto supplyingDepot = new FacilityDto();
    supplyingDepot.setId(supplyingDepotId);
    List<FacilityDto> supplyingDepots = new ArrayList<>(Arrays.asList(supplyingDepot));

    int pageSize = 3;
    int pageNumber = 0;
    Pageable pageable = mock(Pageable.class);

    setupStubsForTestApprovedRequisition(requisitionDtos, filterAndSortBy, filterAndSortBy,
        supplyingDepots, pageable, pageSize, pageNumber);

    requisitionDtos.sort(new RequisitionDtoComparator(filterAndSortBy));
    Collections.reverse(requisitionDtos);
    
    List<RequisitionDto> requisitionDtosSubList =
        requisitionDtos.subList(pageNumber * pageSize, pageNumber * pageSize + pageSize);

    List<RequisitionWithSupplyingDepotsDto> requisitionWithSupplyingDepotDtos =
        requisitionDtosSubList
            .stream()
            .map(requisitionDto ->
                new RequisitionWithSupplyingDepotsDto(requisitionDto, supplyingDepots))
            .collect(Collectors.toList());

    Collection<UUID> userManagedFacilities = new ArrayList<>(Arrays.asList(supplyingDepotId));

    //when
    Page<RequisitionWithSupplyingDepotsDto> requisitionDtosRetrieved =
        requisitionService.searchApprovedRequisitionsWithSortAndFilterAndPaging(null,
            filterAndSortBy, filterAndSortBy, true, pageable, userManagedFacilities);

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
  public void shouldConvertRequisitionsToOrders() {
    // given
    int requisitionsCount = 5;

    UserDto user = mock(UserDto.class);
    when(user.getId()).thenReturn(UUID.randomUUID());

    List<ConvertToOrderDto> list = setUpReleaseRequisitionsAsOrder(requisitionsCount);

    List<FacilityDto> facilities = list.stream()
        .map(r -> facilityReferenceDataService.findOne(r.getSupplyingDepotId()))
        .collect(Collectors.toList());

    when(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(user.getId(),
        convertToOrderRightId)).thenReturn(facilities);

    // when
    requisitionService.convertToOrder(list, user);

    // then
    verify(orderFulfillmentService).create(any(List.class));
  }


  @Test(expected = ValidationMessageException.class)
  public void shouldNotSwallowExceptionsFromFulfillmentService()
      throws ConfigurationSettingException {
    // given
    int requisitionsCount = 5;

    UserDto user = mock(UserDto.class);
    when(user.getId()).thenReturn(UUID.randomUUID());

    List<ConvertToOrderDto> list = setUpReleaseRequisitionsAsOrder(requisitionsCount);

    List<FacilityDto> facilities = list.stream()
        .map(r -> facilityReferenceDataService.findOne(r.getSupplyingDepotId()))
        .collect(Collectors.toList());

    when(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(user.getId(),
        convertToOrderRightId)).thenReturn(facilities);

    doThrow(new ValidationMessageException(("test"))).when(
      orderFulfillmentService).create(any(List.class));

    // when
    requisitionService.convertToOrder(list, user);
  }

  @Test
  public void shouldProcessStatusChangeWhenConvertingRequisitionToOrder() throws Exception {
    UserDto user = mock(UserDto.class);
    when(user.getId()).thenReturn(UUID.randomUUID());

    List<ConvertToOrderDto> list = setUpReleaseRequisitionsAsOrder(1);

    List<FacilityDto> facilities = list.stream()
        .map(r -> facilityReferenceDataService.findOne(r.getSupplyingDepotId()))
        .collect(Collectors.toList());

    when(fulfillmentFacilitiesReferenceDataService.getFulfillmentFacilities(user.getId(),
        convertToOrderRightId)).thenReturn(facilities);

    requisitionService.convertToOrder(list, user);

    verify(requisitionStatusProcessor).statusChange(any(Requisition.class));
  }

  @Test
  public void shouldReturnFullSupplyRequisitionLineItems() {
    // given
    Requisition requisition = generateRequisition();
    when(requisitionRepository.findOne(requisition.getId())).thenReturn(requisition);

    List<RequisitionLineItem> fullSupply = Collections.singletonList(lineItem1);
    List<RequisitionLineItem> nonFullSupply = Collections.singletonList(lineItem2);

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

    List<RequisitionLineItem> fullSupply = Collections.singletonList(lineItem1);
    List<RequisitionLineItem> nonFullSupply = Collections.singletonList(lineItem2);

    setupStubsForTestFindSupplyItems(requisition, fullSupply, nonFullSupply);

    // when
    List<RequisitionLineItem> result = requisitionService
        .getNonFullSupplyItems(requisition.getId());

    Set<RequisitionLineItem> resultSet = new HashSet<>(result);
    Set<RequisitionLineItem> nonFullSupplySet = new HashSet<>(nonFullSupply);

    // then
    assertTrue(resultSet.equals(nonFullSupplySet));
  }
  
  private List<ConvertToOrderDto> setUpReleaseRequisitionsAsOrder(int amount) {
    if (amount < 1) {
      throw new IllegalArgumentException("Amount must be a positive number");
    }

    List<ConvertToOrderDto> result = new ArrayList<>();

    for (int i = 0; i < amount; i++) {
      FacilityDto facility = mock(FacilityDto.class);
      when(facility.getId()).thenReturn(UUID.randomUUID());

      Requisition requisition = new Requisition(UUID.randomUUID(), UUID.randomUUID(),
          UUID.randomUUID(), APPROVED, false);
      requisition.setId(UUID.randomUUID());
      requisition.setSupervisoryNodeId(UUID.randomUUID());
      requisition.setSupplyingFacilityId(facility.getId());
      requisition.setRequisitionLineItems(Lists.newArrayList());

      when(requisitionRepository.findOne(requisition.getId())).thenReturn(requisition);
      when(facilityReferenceDataService.findOne(facility.getId())).thenReturn(facility);
      when(facilityReferenceDataService
          .searchSupplyingDepots(requisition.getProgramId(), requisition.getSupervisoryNodeId()))
          .thenReturn(Collections.singletonList(facility));

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

  private SupplyLineDto generateSupplyLine(
      UUID program, UUID supervisoryNode, UUID facility) {
    SupplyLineDto supplyLine = new SupplyLineDto();
    supplyLine.setProgram(program);
    supplyLine.setSupervisoryNode(supervisoryNode);
    supplyLine.setSupplyingFacility(facility);
    return supplyLine;
  }

  private List<RequisitionDto> getRequisitionDtoList() {
    List<RequisitionDto> requisitionDtos = new ArrayList<>();
    String[] programNames = {"one", "two", "three", "four", "five"};

    for (String programName : programNames) {
      RequisitionDto requisitionDto = new RequisitionDto();
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
    ProgramOrderableDto fullSupplyProduct = mock(ProgramOrderableDto.class);
    when(fullSupplyProduct.getProgramId()).thenReturn(requisition.getProgramId());
    when(fullSupplyProduct.getFullSupply()).thenReturn(true);

    ProgramOrderableDto nonFullSupplyProduct = mock(ProgramOrderableDto.class);
    when(nonFullSupplyProduct.getProgramId()).thenReturn(requisition.getProgramId());
    when(nonFullSupplyProduct.getFullSupply()).thenReturn(false);

    OrderableDto fullSupplyOrderable = mock(OrderableDto.class);
    UUID fullSupplyLineProductId = UUID.randomUUID();
    when(orderableReferenceDataService.findOne(fullSupplyLineProductId))
        .thenReturn(fullSupplyOrderable);
    when(fullSupplyOrderable.getPrograms())
        .thenReturn(Collections.singleton(fullSupplyProduct));

    OrderableDto nonFullSupplyOrderable = mock(OrderableDto.class);
    UUID nonFullSupplyLineProductId = UUID.randomUUID();
    when(orderableReferenceDataService.findOne(nonFullSupplyLineProductId))
        .thenReturn(nonFullSupplyOrderable);
    when(nonFullSupplyOrderable.getPrograms())
        .thenReturn(Collections.singleton(nonFullSupplyProduct));

    fullSupply.forEach(line -> when(line.getOrderableId())
        .thenReturn(fullSupplyLineProductId));
    nonFullSupply.forEach(line -> when(line.getOrderableId())
        .thenReturn(nonFullSupplyLineProductId));
  }

  private void setupStubsForTestApprovedRequisition(List<RequisitionDto> requisitionDtos,
                                                    String filterBy, String programName,
                                                    List<FacilityDto> supplyingDepots,
                                                    Pageable pageable, int pageSize,
                                                    int pageNumber) {
    List<UUID> desiredUuids = new ArrayList<>();
    List<Requisition> requisitions = new ArrayList<>();
    when(programReferenceDataService.search(programName))
        .thenReturn(Collections.emptyList());
    when(requisitionRepository.searchApprovedRequisitions(filterBy, desiredUuids))
        .thenReturn(requisitions);
    when(convertHelper.convertRequisitionListToRequisitionDtoList(requisitions))
        .thenReturn(requisitionDtos);
    when(requisitionRepository.findOne(any())).thenReturn(mock(Requisition.class));
    when(facilityReferenceDataService.searchSupplyingDepots(any(), any()))
        .thenReturn(supplyingDepots);
    when(pageable.getPageSize()).thenReturn(pageSize);
    when(pageable.getPageNumber()).thenReturn(pageNumber);
  }

  private RequisitionTemplate getRequisitionTemplate() {
    RequisitionTemplateColumn column = new RequisitionTemplateColumn();
    //column.setSetting(SETTING);

    RequisitionTemplateColumn beginningBalanceColumn = new RequisitionTemplateColumn();
    beginningBalanceColumn.setName("beginningBalance");
    beginningBalanceColumn.setIsDisplayed(true);

    Map<String, RequisitionTemplateColumn> columnsMap = new HashMap<>();
    columnsMap.put(AVERAGE_CONSUMPTION, column);
    columnsMap.put(BEGINNING_BALANCE, beginningBalanceColumn);

    RequisitionTemplate requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setColumnsMap(columnsMap);
    return requisitionTemplate;
  }

  private void mockPreviousRequisition() {
    RequisitionLineItem previousRequisitionLineItem = new RequisitionLineItem();
    previousRequisitionLineItem.setAdjustedConsumption(ADJUSTED_CONSUMPTION);
    previousRequisitionLineItem.setOrderableId(PRODUCT_ID);
    Requisition previousRequisition = new Requisition();
    previousRequisition.setId(UUID.randomUUID());
    previousRequisition
        .setRequisitionLineItems(Collections.singletonList(previousRequisitionLineItem));

    when(requisitionRepository
        .searchRequisitions(any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(Collections.singletonList(previousRequisition));
  }

  private void mockNoPreviousRequisition() {
    when(requisitionRepository
        .searchRequisitions(any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(Collections.emptyList());
  }

  private void mockApprovedProduct(UUID productId, boolean fullSupply) {
    ProgramDto program = new ProgramDto();
    program.setId(UUID.randomUUID());

    ProgramOrderableDto product = new ProgramOrderableDto();
    product.setOrderableId(productId);
    product.setProgramId(program.getId());
    product.setPricePerPack(Money.of(CurrencyUnit.USD, 1));

    OrderableDto orderable = new OrderableDto();
    orderable.setId(productId);
    orderable.setPrograms(Sets.newHashSet(product));

    ApprovedProductDto approvedProductDto = new ApprovedProductDto();
    approvedProductDto.setId(PRODUCT_ID);
    approvedProductDto.setOrderable(orderable);
    approvedProductDto.setProgram(program);
    approvedProductDto.setMaxPeriodsOfStock(7.25);

    when(approvedProductReferenceDataService.getApprovedProducts(any(), any(), eq(fullSupply)))
        .thenReturn(Collections.singletonList(approvedProductDto));
  }

  private void mockNonFullSupplyApprovedProduct() {
    mockApprovedProduct(NON_FULL_PRODUCT_ID, false);
  }

  private void prepareForPodTest() {
    prepareForTestInitiate(SETTING);
    mockApprovedProduct(PRODUCT_ID, true);
    RequisitionTemplate requisitionTemplate = mock(RequisitionTemplate.class);
    when(requisitionTemplate.hasColumnsDefined()).thenReturn(true);
    when(requisitionTemplate.getNumberOfPeriodsToAverage()).thenReturn(2);

    when(requisitionRepository.findOne(requisition.getId())).thenReturn(null);

    when(facilityReferenceDataService.findOne(facilityId)).thenReturn(mock(FacilityDto.class));
    when(programReferenceDataService.findOne(programId)).thenReturn(mock(ProgramDto.class));
    doReturn(requisitionTemplate).when(requisitionTemplateService).getTemplateForProgram(programId);

    ProcessingPeriodDto periodDto = new ProcessingPeriodDto();
    periodDto.setStartDate(LocalDate.of(2016, 11, 1));
    periodDto.setEndDate(LocalDate.of(2016, 11, 30));
    periodDto.setDurationInMonths(1);
    doReturn(periodDto).when(periodService).findPeriod(programId, facilityId, suggestedPeriodId,
        false);
  }

  private void mockSupervisionRoleAssignmentForCanApprove(UUID program, UUID node,
                                                          RightDto right) {
    DetailedRoleAssignmentDto detailedRoleAssignmentDto = mock(DetailedRoleAssignmentDto.class);
    when(detailedRoleAssignmentDto.getProgramId()).thenReturn(program);
    when(detailedRoleAssignmentDto.getSupervisoryNodeId()).thenReturn(node);
    when(detailedRoleAssignmentDto.getRole()).thenReturn(role);

    Set<RightDto> rights = new HashSet<>();
    rights.add(right);
    when(role.getRights()).thenReturn(rights);

    when(userRoleAssignmentsReferenceDataService.getRoleAssignments(userId))
        .thenReturn(Sets.newHashSet(detailedRoleAssignmentDto));
  }

  private void prepareForTestInitiate(Integer numberOfPeriodsToAverage) {
    RequisitionTemplate requisitionTemplate = mock(RequisitionTemplate.class);
    when(requisitionTemplate.hasColumnsDefined()).thenReturn(true);
    when(requisitionTemplate.getNumberOfPeriodsToAverage()).thenReturn(numberOfPeriodsToAverage);

    when(requisitionRepository.findOne(requisition.getId())).thenReturn(null);

    when(facilityReferenceDataService.findOne(facilityId)).thenReturn(mock(FacilityDto.class));
    when(programReferenceDataService.findOne(programId)).thenReturn(mock(ProgramDto.class));
    when(requisitionTemplateService.getTemplateForProgram(programId))
        .thenReturn(requisitionTemplate);

    ProcessingPeriodDto periodDto = new ProcessingPeriodDto();
    periodDto.setDurationInMonths(1);
    when(periodService.findPeriod(programId, facilityId, suggestedPeriodId, false))
        .thenReturn(periodDto);
  }

  private List<Requisition> mockSearchRequisitionsForApproval() {
    requisition.setStatus(IN_APPROVAL);
    List<Requisition> requisitions = new ArrayList<>();
    requisitions.add(requisition);
    Requisition requisition2 = generateRequisition();
    requisition2.setStatus(AUTHORIZED);
    requisitions.add(requisition2);

    when(requisitionRepository.searchRequisitions(
        null, programId, null, null, null, supervisoryNodeId, null, null))
        .thenReturn(requisitions);
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

    when(convertToOrderRight.getId()).thenReturn(convertToOrderRightId);
    when(approveRequisitionRight.getId()).thenReturn(approveRequisitionRightId);
    when(facility.getId()).thenReturn(facilityId);
    when(program.getId()).thenReturn(programId);
    when(supervisoryNode.getId()).thenReturn(supervisoryNodeId);

    processingPeriodDto = new ProcessingPeriodDto();
    processingPeriodDto.setProcessingSchedule(processingScheduleDto);
    processingPeriodDto.setId(suggestedPeriodId);
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
}
