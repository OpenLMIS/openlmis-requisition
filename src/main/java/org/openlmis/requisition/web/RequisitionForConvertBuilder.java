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

import com.google.common.collect.Lists;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.dto.BasicRequisitionDto;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.MinimalFacilityDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionWithSupplyingDepotsDto;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Component;

@Component
public class RequisitionForConvertBuilder {

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private BasicRequisitionDtoBuilder basicRequisitionDtoBuilder;

  /**
   * Builds representation of the requisitions that are ready for converting to an order,
   * containing supplying facilities that the user has rights to.
   *
   * @param requisitions          the list of requisitions to build for
   * @param userManagedFacilities UUIDs of supplying depots the user has rights to
   * @param minimalFacilities     filtered facilities for creating requisition dto
   * @param programs              filtered programs for creating requisition dto
   * @return a list of requisition with supplying depots representation
   */
  public List<RequisitionWithSupplyingDepotsDto> buildRequisitions(Page<Requisition> requisitions,
                                                 Collection<UUID> userManagedFacilities,
                                                 Map<UUID, MinimalFacilityDto> minimalFacilities,
                                                 Map<UUID, ProgramDto> programs) {

    List<RequisitionWithSupplyingDepotsDto> responseList = new ArrayList<>();
    Map<RightsFor, List<FacilityDto>> cache = new HashMap<>();

    for (Requisition requisition : requisitions) {
      List<FacilityDto> facilities = getAvailableSupplyingDepots(requisition, cache)
          .stream()
          .filter(f -> userManagedFacilities.contains(f.getId()))
          .collect(Collectors.toList());

      if (!facilities.isEmpty()) {
        BasicRequisitionDto requisitionDto = basicRequisitionDtoBuilder.build(requisition,
            minimalFacilities.get(requisition.getFacilityId()),
            programs.get(requisition.getProgramId()));
        responseList.add(new RequisitionWithSupplyingDepotsDto(requisitionDto, facilities));
      }
    }

    return responseList;
  }


  /**
   * Retrieves available supplying depots for given requisition.
   *
   * @param requisitionId id of requisition to find facilities for
   * @return list of facilities
   */
  public List<FacilityDto> getAvailableSupplyingDepots(UUID requisitionId) {
    return getAvailableSupplyingDepotsForRequisition(requisitionRepository.findOne(requisitionId));
  }

  /**
   * Retrieves available supplying depots for given requisition.
   *
   * @param requisition requisition to find facilities for
   * @param cache a map containing supplying depots for rights that were already processed
   *
   * @return list of facilities
   */
  public List<FacilityDto> getAvailableSupplyingDepots(Requisition requisition,
                                                       Map<RightsFor, List<FacilityDto>> cache) {
    List<FacilityDto> supplyingFacilities = getCached(cache, requisition);
    if (supplyingFacilities == null) {
      Collection<FacilityDto> facilityDtos = getAvailableSupplyingDepotsForRequisition(requisition);

      supplyingFacilities = Lists.newArrayList(facilityDtos);
      addCached(cache, requisition, supplyingFacilities);
    }

    return supplyingFacilities;
  }

  private List<FacilityDto> getAvailableSupplyingDepotsForRequisition(Requisition requisition) {
    Collection<FacilityDto> facilityDtos = facilityReferenceDataService
        .searchSupplyingDepots(requisition.getProgramId(), requisition.getSupervisoryNodeId());

    return Lists.newArrayList(facilityDtos);
  }


  private List<FacilityDto> getCached(Map<RightsFor, List<FacilityDto>> cache,
                                      Requisition requisition) {
    return cache.get(
        new RightsFor(requisition.getProgramId(), requisition.getSupervisoryNodeId()));
  }

  private void addCached(Map<RightsFor, List<FacilityDto>> cache,
                              Requisition requisition, List<FacilityDto> facilities) {
    cache.put(
        new RightsFor(requisition.getProgramId(), requisition.getSupervisoryNodeId()), facilities);
  }

  @AllArgsConstructor
  @EqualsAndHashCode
  private class RightsFor {
    private UUID program;
    private UUID supervisoryNode;
  }
}
