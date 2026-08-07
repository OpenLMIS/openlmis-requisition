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

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.stockmanagement.StockCardSummaryDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * Fetches supplying-facility stock on hand and aggregates it per orderable: sum across each
 * facility's lots, then maximum across facilities (the largest single depot that could supply the
 * line). The cross-service call uses the service account, so no facility-scoped token is needed.
 * Any single lookup failure yields no values at all, so approvers never see an under-counted total.
 */
@Component
@RequiredArgsConstructor
public class SupplyingFacilitySohAggregator {

  private static final Logger LOGGER =
      LoggerFactory.getLogger(SupplyingFacilitySohAggregator.class);

  private final StockCardSummariesStockManagementService stockCardSummariesService;

  /**
   * Aggregates supplying-facility SOH per orderable.
   *
   * @return {@code Optional.empty()} when any per-facility lookup failed (all SOH treated as null);
   *         otherwise a map of orderable id to the max-across-facilities of the per-facility lot
   *         sums. An orderable absent from the map has no stock card (line SOH is null), which is
   *         distinct from a mapped value of {@code 0} (a card exists but is empty).
   */
  public Optional<Map<UUID, Integer>> aggregate(UUID programId, List<FacilityDto> facilities,
      Set<UUID> orderableIds) {
    if (facilities.isEmpty() || orderableIds.isEmpty()) {
      return Optional.of(Collections.emptyMap());
    }

    Map<UUID, Integer> byOrderable = new HashMap<>();
    for (FacilityDto facility : facilities) {
      try {
        perFacilitySoh(programId, facility.getId(), orderableIds)
            .forEach((orderableId, soh) -> byOrderable.merge(orderableId, soh, Integer::max));
      } catch (RuntimeException ex) {
        LOGGER.warn("Supplying-facility stock-on-hand lookup failed for facility {}",
            facility.getId(), ex);
        return Optional.empty();
      }
    }
    return Optional.of(byOrderable);
  }

  private Map<UUID, Integer> perFacilitySoh(UUID programId, UUID facilityId,
      Set<UUID> orderableIds) {
    return stockCardSummariesService.search(programId, facilityId, orderableIds, null)
        .stream()
        .filter(card -> card.getOrderable() != null && card.getStockOnHand() != null)
        .collect(Collectors.groupingBy(card -> card.getOrderable().getId(),
            Collectors.summingInt(StockCardSummaryDto::getStockOnHand)));
  }
}
