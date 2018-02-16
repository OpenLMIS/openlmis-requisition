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

import org.apache.commons.lang3.RandomUtils;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProgramDto;

import java.util.UUID;

public class ApprovedProductDtoDataBuilder {
  private UUID id = UUID.randomUUID();
  private OrderableDto orderable = new OrderableDtoDataBuilder().build();
  private ProgramDto program = new ProgramDtoDataBuilder().build();
  private Double maxPeriodsOfStock = 7.25;
  private Double minPeriodsOfStock = RandomUtils.nextDouble();
  private Double emergencyOrderPoint = RandomUtils.nextDouble();

  public ApprovedProductDtoDataBuilder withOrderable(OrderableDto orderable) {
    this.orderable = orderable;
    return this;
  }

  public ApprovedProductDtoDataBuilder withProgram(ProgramDto program) {
    this.program = program;
    return this;
  }

  public ApprovedProductDtoDataBuilder withMaxPeriodsOfStock(double maxPeriodsOfStock) {
    this.maxPeriodsOfStock = maxPeriodsOfStock;
    return this;
  }

  /**
   * Creates new instance of {@link ApprovedProductDto} with properties.
   * @return created approved product dto
   */
  public ApprovedProductDto build() {
    return new ApprovedProductDto(
        id, orderable, program, maxPeriodsOfStock, minPeriodsOfStock, emergencyOrderPoint
    );
  }
}
