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

import java.util.Objects;

import lombok.Getter;
import lombok.Setter;
import org.openlmis.requisition.domain.RejectionReasonCategory;

public class RejectionReasonCategoryDto extends BaseDto implements
        RejectionReasonCategory.Exporter, RejectionReasonCategory.Importer {

  @Getter
  @Setter
  private String name;

  @Getter
  @Setter
  private String code;

  @Getter
  @Setter
  private Boolean active;

  @Override
  public int hashCode() {
    return name.hashCode();
  }

  /**
   * Creates new instance of {@link RejectionReasonCategoryDto} based on passed rejection reason
   * category.
   */
  public static RejectionReasonCategoryDto newInstance(
          RejectionReasonCategory rejectionReasonCategory) {
    RejectionReasonCategoryDto dto = new RejectionReasonCategoryDto();
    rejectionReasonCategory.export(dto);

    return dto;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (!(obj instanceof RejectionReasonCategoryDto)) {
      return false;
    }
    RejectionReasonCategoryDto rejectionReasonCategoryDto = (RejectionReasonCategoryDto) obj;
    return Objects.equals(name, rejectionReasonCategoryDto.name);
  }
}
