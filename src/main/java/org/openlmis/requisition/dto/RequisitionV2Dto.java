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

import java.util.List;
import java.util.Set;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public final class RequisitionV2Dto extends BaseRequisitionDto {

  @Getter
  @Setter
  private ObjectReferenceDto facility;

  @Getter
  @Setter
  private ObjectReferenceDto program;

  @Getter
  @Setter
  private ObjectReferenceDto processingPeriod;

  @Setter
  private List<RequisitionLineItemV2Dto> requisitionLineItems;

  @Getter
  @Setter
  private Set<VersionObjectReferenceDto> availableFullSupplyProducts;

  @Getter
  @Setter
  private Set<VersionObjectReferenceDto> availableNonFullSupplyProducts;

  @Override
  List<? extends BaseRequisitionLineItemDto> getLineItems() {
    return requisitionLineItems;
  }
}
