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

import com.google.common.collect.Maps;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.ProgramOrderableDto;

public final class ApproveProductsAggregator {
  private Map<UUID, ApprovedProductDto> allProducts;
  private Map<UUID, ApprovedProductDto> fullSupplyProducts;
  private Map<UUID, ApprovedProductDto> nonFullSupplyProducts;

  /**
   * Create a new instance of the {@link ApproveProductsAggregator}.
   */
  public ApproveProductsAggregator(List<ApprovedProductDto> products, UUID programId) {
    allProducts = Maps.newHashMap();
    fullSupplyProducts = Maps.newHashMap();
    nonFullSupplyProducts = Maps.newHashMap();
    groupByOrderableId(products, programId);
  }

  private void groupByOrderableId(List<ApprovedProductDto> products, UUID programId) {
    for (ApprovedProductDto approvedProduct : products) {
      OrderableDto orderable = approvedProduct.getOrderable();
      ProgramOrderableDto po = orderable.findProgramOrderableDto(programId);

      if (null != po) {
        allProducts.put(approvedProduct.getOrderable().getId(), approvedProduct);

        if (Objects.equals(true, po.getFullSupply())) {
          fullSupplyProducts.put(approvedProduct.getOrderable().getId(), approvedProduct);
        }

        if (Objects.equals(false, po.getFullSupply())) {
          nonFullSupplyProducts.put(approvedProduct.getOrderable().getId(), approvedProduct);
        }
      }
    }
  }

  public Set<UUID> getOrderableIds() {
    return allProducts.keySet();
  }

  public Set<UUID> getNonFullSupplyOrderableIds() {
    return nonFullSupplyProducts.keySet();
  }

  public Collection<ApprovedProductDto> getFullSupplyProducts() {
    return fullSupplyProducts.values();
  }

  public Set<UUID> getFullSupplyOrderableIds() {
    return fullSupplyProducts.keySet();
  }

  public ApprovedProductDto getFullSupplyProduct(UUID orderableId) {
    return fullSupplyProducts.get(orderableId);
  }
}
