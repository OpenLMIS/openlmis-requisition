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

import java.util.UUID;
import org.apache.commons.lang.RandomStringUtils;
import org.openlmis.requisition.dto.SupplyLineDto;

public class SupplyLineDtoDataBuilder {
  private UUID id = UUID.randomUUID();
  private UUID supervisoryNode = UUID.randomUUID();
  private String description = RandomStringUtils.randomAlphanumeric(5);
  private UUID program = UUID.randomUUID();
  private UUID supplyingFacility = UUID.randomUUID();

  /**
   * Creates new instance of {@link SupplyLineDto}.
   */
  public SupplyLineDto build() {
    SupplyLineDto dto = new SupplyLineDto(
        supervisoryNode, description, program, supplyingFacility
    );
    dto.setId(id);
    return dto;
  }
}
