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

import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@AllArgsConstructor
@NoArgsConstructor
@ToString
@EqualsAndHashCode
public final class StockCardRangeSummaryDto {

  @Getter
  @Setter
  private ObjectReferenceDto orderable;

  @Getter
  @Setter
  private Long stockOutDays;

  @Getter
  @Setter
  private Map<String, Integer> tags;

  /**
   * Sums amount values from tags map.
   *
   * @param tag tag name to get it's value
   * @return sum of all amount values
   */
  public Integer getAmount(String tag) {
    return tags.entrySet().stream()
        .filter(map -> map.getKey().matches(tag))
        .map(Map.Entry::getValue)
        .findFirst().orElse(0);
  }

}
