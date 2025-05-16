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

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.internal.verification.VerificationModeFactory.atLeastOnce;

import com.google.common.collect.Maps;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.dto.ReleasableRequisitionBatchDto;
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
import org.openlmis.requisition.service.referencedata.SupervisoryNodeReferenceDataService;
import org.openlmis.requisition.service.referencedata.SupplyLineReferenceDataService;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.openlmis.requisition.service.stockmanagement.StockEventStockManagementService;
import org.openlmis.requisition.testutils.DtoGenerator;
import org.openlmis.requisition.testutils.ReleasableRequisitionBatchDtoDataBuilder;
import org.openlmis.requisition.utils.AuthenticationHelper;
import org.openlmis.requisition.utils.StockEventBuilder;
import org.springframework.test.util.ReflectionTestUtils;

public class BatchRequisitionControllerTest {

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
  public void approveMultipleRequisitionsWhenTransferStockDataIsEnabled() {
    ReflectionTestUtils.setField(batchRequisitionController,
            "isTransferStockDataFromRequisitionToStockManagementEnabled", true);
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
    verify(stockEventBuilder).fromRequisition(requisition, currentUser.getId(), Maps.newHashMap());
    verify(stockEventStockManagementService).submit(stockEventDto);
  }

  @Test
  public void approveMultipleRequisitionsWhenTransferStockDataIsDisabled() {
    ReflectionTestUtils.setField(batchRequisitionController,
            "isTransferStockDataFromRequisitionToStockManagementEnabled", false);
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
}
