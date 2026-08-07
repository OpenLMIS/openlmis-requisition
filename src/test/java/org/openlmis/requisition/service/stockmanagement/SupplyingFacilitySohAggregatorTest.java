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

package org.openlmis.requisition.service.stockmanagement;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Collections.singleton;
import static java.util.Collections.singletonList;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.stockmanagement.StockCardSummaryDto;
import org.openlmis.requisition.testutils.FacilityDtoDataBuilder;
import org.openlmis.requisition.testutils.StockCardSummaryDtoDataBuilder;

@RunWith(MockitoJUnitRunner.class)
public class SupplyingFacilitySohAggregatorTest {

  @Mock
  private StockCardSummariesStockManagementService stockCardSummariesService;

  private SupplyingFacilitySohAggregator aggregator;

  private final UUID programId = UUID.randomUUID();
  private final UUID orderableX = UUID.randomUUID();
  private final UUID orderableY = UUID.randomUUID();

  @Before
  public void setUp() {
    aggregator = new SupplyingFacilitySohAggregator(stockCardSummariesService);
  }

  private FacilityDto facility() {
    return new FacilityDtoDataBuilder().buildAsDto();
  }

  private StockCardSummaryDto card(UUID orderableId, Integer stockOnHand) {
    StockCardSummaryDto dto = new StockCardSummaryDtoDataBuilder()
        .withOrderableId(orderableId)
        .buildAsDto();
    dto.setStockOnHand(stockOnHand);
    return dto;
  }

  @Test
  public void shouldReturnEmptyMapWithoutCallingStockServiceWhenNoFacilities() {
    Optional<Map<UUID, Integer>> result =
        aggregator.aggregate(programId, emptyList(), singleton(orderableX));

    assertThat(result.isPresent(), is(true));
    assertThat(result.get().isEmpty(), is(true));
    verifyNoInteractions(stockCardSummariesService);
  }

  @Test
  public void shouldSumStockOnHandAcrossLotsWithinFacility() {
    FacilityDto facility = facility();
    when(stockCardSummariesService.search(eq(programId), eq(facility.getId()), anySet(), isNull()))
        .thenReturn(asList(card(orderableX, 30), card(orderableX, 70)));

    Map<UUID, Integer> soh =
        aggregator.aggregate(programId, singletonList(facility), singleton(orderableX)).get();

    assertThat(soh.get(orderableX), is(100));
  }

  @Test
  public void shouldTakeMaxAcrossFacilities() {
    FacilityDto facilityA = facility();
    FacilityDto facilityB = facility();
    when(stockCardSummariesService.search(eq(programId), eq(facilityA.getId()), anySet(), isNull()))
        .thenReturn(singletonList(card(orderableX, 300)));
    when(stockCardSummariesService.search(eq(programId), eq(facilityB.getId()), anySet(), isNull()))
        .thenReturn(singletonList(card(orderableX, 250)));

    Map<UUID, Integer> soh =
        aggregator.aggregate(programId, asList(facilityA, facilityB), singleton(orderableX)).get();

    assertThat(soh.get(orderableX), is(300));
  }

  @Test
  public void shouldPreserveZeroAndOmitNullStockOnHand() {
    FacilityDto facility = facility();
    when(stockCardSummariesService.search(eq(programId), eq(facility.getId()), anySet(), isNull()))
        .thenReturn(asList(card(orderableX, 0), card(orderableY, null)));

    Set<UUID> orderableIds = new HashSet<>(asList(orderableX, orderableY));
    Map<UUID, Integer> soh =
        aggregator.aggregate(programId, singletonList(facility), orderableIds).get();

    assertThat(soh.get(orderableX), is(0));
    assertThat(soh.containsKey(orderableY), is(false));
  }

  @Test
  public void shouldReturnEmptyWhenAnyFacilityLookupFails() {
    FacilityDto facilityA = facility();
    FacilityDto facilityB = facility();
    when(stockCardSummariesService.search(eq(programId), eq(facilityA.getId()), anySet(), isNull()))
        .thenReturn(singletonList(card(orderableX, 100)));
    when(stockCardSummariesService.search(eq(programId), eq(facilityB.getId()), anySet(), isNull()))
        .thenThrow(new IllegalStateException("stock service down"));

    Optional<Map<UUID, Integer>> result =
        aggregator.aggregate(programId, asList(facilityA, facilityB), singleton(orderableX));

    assertThat(result.isPresent(), is(false));
  }

  @Test
  public void shouldReturnEmptyWhenSingleFacilityLookupFails() {
    FacilityDto facility = facility();
    when(stockCardSummariesService.search(eq(programId), eq(facility.getId()), anySet(), isNull()))
        .thenThrow(new IllegalStateException("stock service down"));

    Optional<Map<UUID, Integer>> result =
        aggregator.aggregate(programId, singletonList(facility), singleton(orderableX));

    assertThat(result.isPresent(), is(false));
  }
}
