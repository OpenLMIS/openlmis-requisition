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

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyList;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.internal.verification.VerificationModeFactory.atLeastOnce;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_NO_REQUISITIONS_TO_APPROVE;
import static org.openlmis.requisition.service.PermissionService.REQUISITION_APPROVE;

import com.google.common.collect.Maps;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArrayList;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.RequisitionTemplateColumnDataBuilder;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.domain.requisition.RequisitionLineItemDataBuilder;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.dto.ReleasableRequisitionBatchDto;
import org.openlmis.requisition.dto.RequisitionsProcessingStatusDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.dto.stockmanagement.StockEventDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.FacilityTypeApprovedProductReferenceDataService;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.RightReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupplyLineReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserRoleAssignmentsReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.StockEventStockManagementService;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.testutils.ReleasableRequisitionBatchDtoDataBuilder;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.utils.StockEventBuilder;
import org.slf4j.profiler.Profiler;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

public class BatchRequisitionControllerTest {

  private static final String TRANSFER_STOCK_DATA_FLAG =
      "isTransferStockDataFromRequisitionToStockManagementEnabled";

  @Mock
  RequisitionService requisitionService;

  @Mock
  AuthenticationHelper authenticationHelper;

  @Mock
  PermissionService permissionService;

  @Mock
  RequisitionRepository requisitionRepository;

  @Mock
  UserReferenceDataService userReferenceDataService;

  @Mock
  SupervisoryNodeReferenceDataService supervisoryNodeReferenceDataService;

  @Mock
  OrderableReferenceDataService orderableReferenceDataService;

  @Mock
  FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  PeriodReferenceDataService periodReferenceDataService;

  @Mock
  FacilityTypeApprovedProductReferenceDataService facilityTypeApprovedService;

  @Mock
  SupplyLineReferenceDataService supplyLineReferenceDataService;

  @Mock
  MessageService messageService;

  @Mock
  StockEventBuilder stockEventBuilder;

  @Mock
  StockEventStockManagementService stockEventStockManagementService;

  @Mock
  RightReferenceDataService rightReferenceDataService;

  @Mock
  UserRoleAssignmentsReferenceDataService userRoleAssignmentsReferenceDataService;

  @Mock
  Requisition requisition;

  @InjectMocks
  private BatchRequisitionController batchRequisitionController;

  private final UUID uuid1 = UUID.fromString("00000000-0000-0000-0000-000000000001");

  private final UUID uuid2 = UUID.fromString("00000000-0000-0000-0000-000000000002");

  private UserDto currentUser;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    currentUser = DtoGenerator.of(UserDto.class);
    when(authenticationHelper.getCurrentUser()).thenReturn(currentUser);
  }

  @Test
  public void batchReleaseRequisitionsWithOrderWhenUserHasPermission() {
    doReturn(ValidationResult.success())
        .when(permissionService).canConvertToOrder(anyList());
    when(requisitionService.convertToOrder(any(), any()))
        .thenReturn(new ArrayList<>());
    when(requisitionService.releaseWithoutOrder(any()))
        .thenReturn(new ArrayList<>());

    ReleasableRequisitionBatchDto releasableBatchDto =
        new ReleasableRequisitionBatchDtoDataBuilder()
            .withCreateOrder(true)
            .buildAsDto();
    batchRequisitionController.batchReleaseRequisitions(releasableBatchDto);

    verify(requisitionService, atLeastOnce()).convertToOrder(any(), any());
    verify(requisitionService, never()).releaseWithoutOrder(any());
  }

  @Test
  public void batchReleaseRequisitionsWithoutOrderWhenUserHasPermission() {
    doReturn(ValidationResult.success())
        .when(permissionService).canConvertToOrder(anyList());
    when(requisitionService.convertToOrder(any(), any()))
        .thenReturn(new ArrayList<>());
    when(requisitionService.releaseWithoutOrder(any()))
        .thenReturn(new ArrayList<>());

    ReleasableRequisitionBatchDto releasableBatchDto =
        new ReleasableRequisitionBatchDtoDataBuilder()
            .withCreateOrder(false)
            .buildAsDto();
    batchRequisitionController.batchReleaseRequisitions(releasableBatchDto);

    verify(requisitionService, never()).convertToOrder(any(), any());
    verify(requisitionService, atLeastOnce()).releaseWithoutOrder(any());
  }

  @Test
  public void shouldNotSubmitStockEventForRequisitionTheBatchRefused() {
    ReflectionTestUtils.setField(batchRequisitionController,
            TRANSFER_STOCK_DATA_FLAG, true);
    when(requisitionRepository.readDistinctByIdIn(anyList()))
        .thenReturn(Collections.singletonList(requisition));
    when(requisition.getStatus()).thenReturn(RequisitionStatus.APPROVED);

    StockEventDto stockEventDto = DtoGenerator.of(StockEventDto.class);
    when(stockEventBuilder.fromRequisition(any(Requisition.class), any(), anyMap()))
            .thenReturn(stockEventDto);

    batchRequisitionController.approve(Arrays.asList(uuid1, uuid2));

    verify(authenticationHelper).getCurrentUser();
    verify(requisitionRepository).readDistinctByIdIn(anyCollection());
    verify(userReferenceDataService).getPermissionStrings(currentUser.getId());
    verify(rightReferenceDataService).findRight(REQUISITION_APPROVE);
    verify(userRoleAssignmentsReferenceDataService).getRoleAssignments(currentUser.getId());
    verify(supervisoryNodeReferenceDataService).findByIds(anyList());
    verify(orderableReferenceDataService).findByIdentities(anySet());
    verify(facilityReferenceDataService).search(anySet());
    verify(periodReferenceDataService).search(anySet());
    verify(facilityTypeApprovedService).findByIdentities(anySet());
    verify(supervisoryNodeReferenceDataService, never()).findByFacility(any());
    verifyNoInteractions(stockEventBuilder, stockEventStockManagementService);
  }

  @Test
  public void shouldSearchSupervisoryNodesOncePerFacility() {
    ReflectionTestUtils.setField(batchRequisitionController, TRANSFER_STOCK_DATA_FLAG, false);
    UUID facilityId = UUID.randomUUID();
    UUID programId = UUID.randomUUID();
    Requisition otherRequisition = mock(Requisition.class);
    for (Requisition approvable : Arrays.asList(requisition, otherRequisition)) {
      when(approvable.getFacilityId()).thenReturn(facilityId);
      when(approvable.getProgramId()).thenReturn(programId);
      when(approvable.getSupervisoryNodeId()).thenReturn(UUID.randomUUID());
      when(approvable.isApprovable()).thenReturn(true);
    }
    when(userReferenceDataService.getPermissionStrings(currentUser.getId())).thenReturn(
        Collections.singletonList(REQUISITION_APPROVE + "|" + facilityId + "|" + programId));
    when(requisitionRepository.readDistinctByIdIn(anyList()))
        .thenReturn(Arrays.asList(requisition, otherRequisition));

    batchRequisitionController.approve(Arrays.asList(uuid1, uuid2));

    verify(supervisoryNodeReferenceDataService, times(1)).findByFacility(facilityId);
  }

  @Test
  public void approveMultipleRequisitionsWhenTransferStockDataIsDisabled() {
    ReflectionTestUtils.setField(batchRequisitionController,
            TRANSFER_STOCK_DATA_FLAG, false);
    when(requisitionRepository.readDistinctByIdIn(anyList()))
            .thenReturn(Collections.singletonList(requisition));
    when(requisition.getStatus()).thenReturn(RequisitionStatus.APPROVED);

    StockEventDto stockEventDto = DtoGenerator.of(StockEventDto.class);
    when(stockEventBuilder.fromRequisition(any(Requisition.class), any(), anyMap()))
            .thenReturn(stockEventDto);

    batchRequisitionController.approve(Arrays.asList(uuid1, uuid2));

    verify(authenticationHelper).getCurrentUser();
    verify(requisitionRepository).readDistinctByIdIn(anyCollection());
    verify(userReferenceDataService).getPermissionStrings(currentUser.getId());
    verify(supervisoryNodeReferenceDataService).findByIds(anyList());
    verify(orderableReferenceDataService).findByIdentities(anySet());
    verify(facilityReferenceDataService).search(anySet());
    verify(periodReferenceDataService).search(anySet());
    verify(facilityTypeApprovedService).findByIdentities(anySet());
    verifyNoInteractions(stockEventBuilder, stockEventStockManagementService);
  }

  @Test
  public void shouldBuildStockEventsOnTheRequestThread() {
    Requisition otherRequisition = mock(Requisition.class);
    when(requisition.getStatus()).thenReturn(RequisitionStatus.APPROVED);
    when(otherRequisition.getStatus()).thenReturn(RequisitionStatus.APPROVED);
    when(requisition.getTemplate()).thenReturn(mock(RequisitionTemplate.class));
    when(otherRequisition.getTemplate()).thenReturn(mock(RequisitionTemplate.class));

    List<Thread> buildThreads = new CopyOnWriteArrayList<>();
    when(stockEventBuilder.fromRequisition(any(Requisition.class), any(), anyMap()))
        .thenAnswer(invocation -> {
          buildThreads.add(Thread.currentThread());
          return DtoGenerator.of(StockEventDto.class);
        });

    batchRequisitionController.submitStockEvents(new Profiler("TEST"), currentUser,
        Arrays.asList(requisition, otherRequisition), Maps.newHashMap());

    assertEquals(Arrays.asList(Thread.currentThread(), Thread.currentThread()), buildThreads);
    verify(stockEventStockManagementService, times(2)).submit(any(StockEventDto.class));
  }

  @Test
  public void shouldNotSubmitStockEventsWhenNoRequisitionProducesOne() {
    when(requisition.getStatus()).thenReturn(RequisitionStatus.AUTHORIZED);

    batchRequisitionController.submitStockEvents(new Profiler("TEST"), currentUser,
        Collections.singletonList(requisition), Maps.newHashMap());

    verifyNoInteractions(stockEventBuilder, stockEventStockManagementService);
  }

  @Test
  public void shouldTellWhyNothingWasApprovedWhenNoneOfGivenIdsMatchesRequisition() {
    // the batch approval screen sends an empty id list when every requisition in the batch was
    // filtered out by its own validation
    ReflectionTestUtils.setField(batchRequisitionController,
        TRANSFER_STOCK_DATA_FLAG, true);
    when(requisitionRepository.readDistinctByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    ResponseEntity<RequisitionsProcessingStatusDto> response =
        batchRequisitionController.approve(Arrays.asList(uuid1, uuid2));

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    verify(messageService).localize(new Message(ERROR_NO_REQUISITIONS_TO_APPROVE));
    verifyNoInteractions(stockEventBuilder, stockEventStockManagementService);
  }

  @Test
  public void shouldNotFailNullingCalculatedFieldsForHiddenSupplyingFacilityStockColumn() {
    // The supplyingFacilityStockOnHand column is read-only and has no line-item property, so a
    // template carrying it hidden must not make the calculated-field nulling reflect on a missing
    // setter (which would throw). See getColumnNameConditions in BatchRequisitionController.
    RequisitionTemplateColumn hiddenColumn = new RequisitionTemplateColumnDataBuilder()
        .withName("supplyingFacilityStockOnHand")
        .withSource(SourceType.SUPPLYING_FACILITY_STOCK)
        .withNotDisplayed()
        .build();
    Map<String, RequisitionTemplateColumn> columns =
        Collections.singletonMap("supplyingFacilityStockOnHand", hiddenColumn);

    RequisitionTemplate template = mock(RequisitionTemplate.class);
    when(template.viewColumns()).thenReturn(columns);

    RequisitionLineItem lineItem = new RequisitionLineItemDataBuilder().build();
    when(requisition.getTemplate()).thenReturn(template);
    when(requisition.getRequisitionLineItems()).thenReturn(Collections.singletonList(lineItem));

    // Fails (throws IllegalArgumentException) without the exception-list entry.
    ReflectionTestUtils.invokeMethod(
        batchRequisitionController, "setNullForCalculatedFields", requisition);
  }
}