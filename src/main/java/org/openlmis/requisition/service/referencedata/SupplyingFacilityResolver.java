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

package org.openlmis.requisition.service.referencedata;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import lombok.RequiredArgsConstructor;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.SupplyLineDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * Resolves the distinct supplying facilities for a requisition using the existing SupplyLine
 * configuration for its (program, current supervisory node). Internal only; not exposed over HTTP.
 */
@Component
@RequiredArgsConstructor
public class SupplyingFacilityResolver {

  private static final Logger LOGGER =
      LoggerFactory.getLogger(SupplyingFacilityResolver.class);

  private final SupplyLineReferenceDataService supplyLineReferenceDataService;
  private final FacilityReferenceDataService facilityReferenceDataService;

  /**
   * Resolves the distinct supplying facilities for the requisition. Returns an empty list when the
   * requisition has no supervisory node or no matching SupplyLine (i.e. "not configured").
   *
   * @param requisition the requisition being read
   * @return distinct supplying facilities, in SupplyLine order
   */
  public List<FacilityDto> resolve(Requisition requisition) {
    if (requisition.getSupervisoryNodeId() == null) {
      return Collections.emptyList();
    }

    Map<UUID, FacilityDto> distinctById = new LinkedHashMap<>();
    for (SupplyLineDto supplyLine : supplyLineReferenceDataService
        .search(requisition.getProgramId(), requisition.getSupervisoryNodeId())) {
      FacilityDto facility = supplyLine.getSupplyingFacility();
      if (facility != null && facility.getId() != null) {
        distinctById.putIfAbsent(facility.getId(), facility);
      }
    }

    if (distinctById.isEmpty()) {
      return Collections.emptyList();
    }

    return withFacilityDetails(distinctById);
  }

  // The SupplyLine only carries a facility reference (id), so code and name come back empty.
  // Fill them in with a bulk lookup. This is display-only and must not fail the read, so keep
  // the references on a lookup failure and fall back per facility when a lookup misses one.
  private List<FacilityDto> withFacilityDetails(Map<UUID, FacilityDto> distinctById) {
    Map<UUID, FacilityDto> detailedById = new LinkedHashMap<>();
    try {
      for (FacilityDto facility : facilityReferenceDataService.search(distinctById.keySet())) {
        detailedById.put(facility.getId(), facility);
      }
    } catch (RuntimeException ex) {
      LOGGER.warn("Could not load supplying-facility details; using SupplyLine references", ex);
      return new ArrayList<>(distinctById.values());
    }

    List<FacilityDto> resolved = new ArrayList<>(distinctById.size());
    for (Map.Entry<UUID, FacilityDto> entry : distinctById.entrySet()) {
      resolved.add(detailedById.getOrDefault(entry.getKey(), entry.getValue()));
    }
    return resolved;
  }
}
