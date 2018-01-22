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

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.Map;
import java.util.Set;
import java.util.UUID;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class OrderableDto extends BasicOrderableDto {

  private Set<ProgramOrderableDto> programs;
  private DispensableDto dispensable;
  private Map<String, String> identifiers;

  @Builder
  private OrderableDto(UUID id, String productCode, String fullProductName, long netContent,
                       long packRoundingThreshold, boolean roundToZero,
                       Set<ProgramOrderableDto> programs, DispensableDto dispensable) {
    super(id, productCode, fullProductName, netContent, packRoundingThreshold, roundToZero);
    this.programs = programs;
    this.dispensable = dispensable;
  }

  /**
   * Find ProgramOrderableDto in programs using programId.
   *
   * @param programId programId
   * @return product
   */
  public ProgramOrderableDto findProgramOrderableDto(UUID programId) {
    if (programs != null) {
      for (ProgramOrderableDto programOrderableDto : programs) {
        if (programOrderableDto.getProgramId().equals(programId)) {
          return programOrderableDto;
        }
      }
    }
    return null;
  }
}
