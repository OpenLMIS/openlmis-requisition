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

package org.openlmis.requisition.testutils;

import org.openlmis.requisition.dto.ProgramDto;

import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

@Getter
@Setter
public class ProgramDtoDataBuilder {

  private UUID id = UUID.randomUUID();
  private String code = "em";
  private String name = "Essential Meds";
  private String description = "Reporting and ordering of the essential medical commodities";
  private Boolean active = true;
  private Boolean periodsSkippable = true;
  private Boolean showNonFullSupplyTab = true;
  private Boolean skipAuthorization = false;
  private Boolean enableDatePhysicalStockCountCompleted = false;

  /**
   * Builds ProgramDto instance with test data.
   */
  public ProgramDto build() {
    ProgramDto dto = new ProgramDto();
    dto.setId(id);
    dto.setCode(code);
    dto.setName(name);
    dto.setDescription(description);
    dto.setActive(active);
    dto.setPeriodsSkippable(periodsSkippable);
    dto.setShowNonFullSupplyTab(showNonFullSupplyTab);
    dto.setSkipAuthorization(skipAuthorization);
    dto.setEnableDatePhysicalStockCountCompleted(enableDatePhysicalStockCountCompleted);
    return dto;
  }

  /**
   * Builds ProgramDto instance with authorization step skipped.
   */
  public ProgramDto buildWithSkippedAuthorizationStep() {
    return withSkipAuthorization(true).build();
  }

  /**
   * Builds ProgramDto instance with authorization step not skipped.
   */
  public ProgramDto buildWithNotSkippedAuthorizationStep() {
    return withSkipAuthorization(false).build();
  }

  /**
   * Sets skipAuthorization flag in this builder.
   */
  public ProgramDtoDataBuilder withSkipAuthorization(boolean skipAuthorization) {
    this.skipAuthorization = skipAuthorization;
    return this;
  }

  public ProgramDtoDataBuilder withId(UUID id) {
    this.id = id;
    return this;
  }
}
