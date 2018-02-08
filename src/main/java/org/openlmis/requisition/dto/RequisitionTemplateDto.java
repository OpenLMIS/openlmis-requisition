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

package org.openlmis.requisition.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;

import org.openlmis.requisition.domain.RequisitionTemplate;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class RequisitionTemplateDto extends BaseRequisitionTemplateDto
    implements RequisitionTemplate.Exporter, RequisitionTemplate.Importer {

  private String name;
  private boolean populateStockOnHandFromStockCards;
  private Map<String, RequisitionTemplateColumnDto> columnsMap;
  private ObjectReferenceDto program;
  private Set<ObjectReferenceDto> facilityTypes;

  @Override
  @JsonIgnore
  public UUID getProgramId() {
    return null == program ? null : program.getId();
  }

  @Override
  @JsonIgnore
  public Set<UUID> getFacilityTypeIds() {
    return Optional.ofNullable(facilityTypes)
        .orElse(Collections.emptySet())
        .stream()
        .map(ObjectReferenceDto::getId)
        .collect(Collectors.toSet());
  }

}
