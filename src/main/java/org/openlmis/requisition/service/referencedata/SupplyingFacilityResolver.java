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
import org.springframework.stereotype.Component;

/**
 * Resolves the distinct supplying facilities for a requisition using the existing SupplyLine
 * configuration for its (program, current supervisory node). Internal only; not exposed over HTTP.
 */
@Component
@RequiredArgsConstructor
public class SupplyingFacilityResolver {

  private final SupplyLineReferenceDataService supplyLineReferenceDataService;

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
      if (facility != null) {
        distinctById.putIfAbsent(facility.getId(), facility);
      }
    }
    return new ArrayList<>(distinctById.values());
  }
}
