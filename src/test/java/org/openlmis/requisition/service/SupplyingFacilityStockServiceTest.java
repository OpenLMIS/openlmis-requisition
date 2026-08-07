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

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.Collections.singletonMap;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.Optional;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.exception.AuthenticationMessageException;
import org.openlmis.requisition.service.referencedata.SupplyingFacilityResolver;
import org.openlmis.requisition.service.stockmanagement.SupplyingFacilitySohAggregator;
import org.openlmis.requisition.testutils.FacilityDtoDataBuilder;
import org.openlmis.requisition.testutils.OrderableDtoDataBuilder;
import org.openlmis.requisition.utils.Message;

@RunWith(MockitoJUnitRunner.class)
public class SupplyingFacilityStockServiceTest {

  private static final String COLUMN = "supplyingFacilityStockOnHand";

  @Mock
  private PermissionService permissionService;

  @Mock
  private SupplyingFacilityResolver resolver;

  @Mock
  private SupplyingFacilitySohAggregator aggregator;

  @InjectMocks
  private SupplyingFacilityStockService service;

  private final UUID programId = UUID.randomUUID();

  private Requisition requisition;
  private RequisitionTemplate template;
  private RequisitionDto dto;
  private RequisitionLineItemDto lineItem;
  private OrderableDto orderable;
  private FacilityDto facility;

  @Before
  public void setUp() {
    template = mock(RequisitionTemplate.class);
    requisition = mock(Requisition.class);
    when(requisition.getTemplate()).thenReturn(template);
    when(requisition.getProgramId()).thenReturn(programId);
    when(requisition.isApprovable()).thenReturn(true);
    when(template.isColumnInTemplateAndDisplayed(COLUMN)).thenReturn(true);
    when(permissionService.canApproveRequisition(requisition))
        .thenReturn(ValidationResult.success());

    orderable = new OrderableDtoDataBuilder().buildAsDto();
    lineItem = new RequisitionLineItemDto();
    lineItem.setOrderable(orderable);
    dto = new RequisitionDto();
    dto.setRequisitionLineItems(singletonList(lineItem));

    facility = new FacilityDtoDataBuilder().buildAsDto();
  }

  private void allowStockCardsView() {
    when(resolver.resolve(requisition)).thenReturn(singletonList(facility));
    when(permissionService.canViewStockCards(facility.getId(), programId))
        .thenReturn(ValidationResult.success());
  }

  @Test
  public void shouldDoNothingWhenRequisitionIsNotApprovable() {
    when(requisition.isApprovable()).thenReturn(false);

    service.enrich(requisition, dto);

    assertNull(dto.getSupplyingFacilities());
    assertNull(dto.getSupplyingFacilityAccessDenied());
    verifyNoInteractions(resolver, aggregator);
  }

  @Test
  public void shouldDoNothingWhenTemplateColumnIsNotEnabled() {
    when(template.isColumnInTemplateAndDisplayed(COLUMN)).thenReturn(false);

    service.enrich(requisition, dto);

    assertNull(dto.getSupplyingFacilities());
    assertNull(dto.getSupplyingFacilityAccessDenied());
    verifyNoInteractions(resolver, aggregator);
  }

  @Test
  public void shouldDoNothingWhenCallerCannotApprove() {
    when(permissionService.canApproveRequisition(requisition))
        .thenReturn(ValidationResult.noPermission("requisition.error.noApprove"));

    service.enrich(requisition, dto);

    assertNull(dto.getSupplyingFacilities());
    assertNull(dto.getSupplyingFacilityAccessDenied());
    verifyNoInteractions(resolver, aggregator);
  }

  @Test
  public void shouldDoNothingWhenNoSupplyingFacilityResolved() {
    when(resolver.resolve(requisition)).thenReturn(emptyList());

    service.enrich(requisition, dto);

    assertNull(dto.getSupplyingFacilities());
    assertNull(dto.getSupplyingFacilityAccessDenied());
    verifyNoInteractions(aggregator);
  }

  @Test
  public void shouldSetAccessDeniedWhenCallerLacksStockCardsView() {
    when(resolver.resolve(requisition)).thenReturn(singletonList(facility));
    when(permissionService.canViewStockCards(facility.getId(), programId))
        .thenReturn(ValidationResult.noPermission("requisition.error.noStockCardsView"));

    service.enrich(requisition, dto);

    assertTrue(dto.getSupplyingFacilityAccessDenied());
    assertNull(dto.getSupplyingFacilities());
    assertNull(lineItem.getSupplyingFacilityStockOnHand());
    verifyNoInteractions(aggregator);
  }

  @Test
  public void shouldDegradeGracefullyWhenStockCardsViewRightIsNotRegistered() {
    when(resolver.resolve(requisition)).thenReturn(singletonList(facility));
    when(permissionService.canViewStockCards(facility.getId(), programId))
        .thenThrow(new AuthenticationMessageException(
            new Message("requisition.error.rightNotFound")));

    service.enrich(requisition, dto);

    assertTrue(dto.getSupplyingFacilityAccessDenied());
    assertNull(dto.getSupplyingFacilities());
    verifyNoInteractions(aggregator);
  }

  @Test
  public void shouldPopulateFacilitiesAndStockOnHandOnSuccess() {
    allowStockCardsView();
    when(aggregator.aggregate(eq(programId), anyList(), anySet()))
        .thenReturn(Optional.of(singletonMap(orderable.getId(), 300)));

    service.enrich(requisition, dto);

    assertThat(dto.getSupplyingFacilities(), hasSize(1));
    assertThat(dto.getSupplyingFacilities().get(0).getId(), is(facility.getId()));
    assertFalse(dto.getSupplyingFacilityAccessDenied());
    assertThat(lineItem.getSupplyingFacilityStockOnHand(), is(300));
  }

  @Test
  public void shouldPopulateFacilitiesButLeaveStockOnHandNullWhenLookupFails() {
    allowStockCardsView();
    when(aggregator.aggregate(eq(programId), anyList(), anySet())).thenReturn(Optional.empty());

    service.enrich(requisition, dto);

    assertThat(dto.getSupplyingFacilities(), hasSize(1));
    assertFalse(dto.getSupplyingFacilityAccessDenied());
    assertNull(lineItem.getSupplyingFacilityStockOnHand());
  }
}
