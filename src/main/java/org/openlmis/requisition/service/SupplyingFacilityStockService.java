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

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.dto.BaseRequisitionDto;
import org.openlmis.requisition.dto.BaseRequisitionLineItemDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.SupplyingFacilityDto;
import org.openlmis.requisition.dto.VersionIdentityDto;
import org.openlmis.requisition.exception.AuthenticationMessageException;
import org.openlmis.requisition.service.referencedata.SupplyingFacilityResolver;
import org.openlmis.requisition.service.stockmanagement.SupplyingFacilitySohAggregator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

/**
 * Populates supplying-facility stock-on-hand fields on a requisition read DTO during approval. The
 * whole flow is gated (approval-eligible status, template column enabled, caller can approve, and
 * the caller holds STOCK_CARDS_VIEW at the resolved supplying facility); unless every gate passes
 * the DTO is left untouched. Display-only: the approve endpoint is never involved.
 */
@Service
@RequiredArgsConstructor
public class SupplyingFacilityStockService {

  static final String COLUMN = "supplyingFacilityStockOnHand";

  private static final Logger LOGGER =
      LoggerFactory.getLogger(SupplyingFacilityStockService.class);

  private final PermissionService permissionService;
  private final SupplyingFacilityResolver resolver;
  private final SupplyingFacilitySohAggregator aggregator;

  /**
   * Adds supplying-facility metadata and per-line stock on hand to the DTO when the requisition is
   * approval-eligible, the template column is enabled and the caller is authorised. A no-op
   * otherwise, so payloads stay unchanged for every non-approval read.
   *
   * @param requisition the requisition being read
   * @param dto         the response DTO to enrich in place
   */
  public void enrich(Requisition requisition, BaseRequisitionDto dto) {
    if (!requisition.isApprovable()
        || !requisition.getTemplate().isColumnInTemplateAndDisplayed(COLUMN)) {
      return;
    }
    // Receiving-side roles must not see supplying-facility stock and get no notification.
    if (!permissionService.canApproveRequisition(requisition).isSuccess()) {
      return;
    }
    List<FacilityDto> facilities = resolver.resolve(requisition);
    if (facilities.isEmpty()) {
      return;
    }
    if (!hasStockCardsView(facilities, requisition.getProgramId())) {
      dto.setSupplyingFacilityAccessDenied(true);
      return;
    }

    dto.setSupplyingFacilities(toSupplyingFacilityDtos(facilities));
    dto.setSupplyingFacilityAccessDenied(false);

    List<RequisitionLineItem.Importer> lineItems = dto.getRequisitionLineItems();
    aggregator.aggregate(requisition.getProgramId(), facilities, orderableIds(lineItems))
        .ifPresent(byOrderable -> populateLineItemStockOnHand(lineItems, byOrderable));
  }

  private boolean hasStockCardsView(List<FacilityDto> facilities, UUID programId) {
    try {
      return facilities.stream().allMatch(facility ->
          permissionService.canViewStockCards(facility.getId(), programId).isSuccess());
    } catch (AuthenticationMessageException ex) {
      // STOCK_CARDS_VIEW is not registered in this deployment; degrade gracefully rather than 500.
      LOGGER.warn("Could not evaluate STOCK_CARDS_VIEW; supplying-facility stock not shown", ex);
      return false;
    }
  }

  private List<SupplyingFacilityDto> toSupplyingFacilityDtos(List<FacilityDto> facilities) {
    return facilities.stream()
        .map(facility ->
            new SupplyingFacilityDto(facility.getId(), facility.getCode(), facility.getName()))
        .collect(Collectors.toList());
  }

  private Set<UUID> orderableIds(List<RequisitionLineItem.Importer> lineItems) {
    return lineItems.stream()
        .map(RequisitionLineItem.Importer::getOrderableIdentity)
        .filter(Objects::nonNull)
        .map(VersionIdentityDto::getId)
        .collect(Collectors.toSet());
  }

  private void populateLineItemStockOnHand(List<RequisitionLineItem.Importer> lineItems,
      Map<UUID, Integer> byOrderable) {
    for (RequisitionLineItem.Importer importer : lineItems) {
      VersionIdentityDto orderable = importer.getOrderableIdentity();
      if (orderable != null) {
        // The only Importer implementor is BaseRequisitionLineItemDto, which owns the setter.
        BaseRequisitionLineItemDto line = (BaseRequisitionLineItemDto) importer;
        line.setSupplyingFacilityStockOnHand(byOrderable.get(orderable.getId()));
      }
    }
  }
}
