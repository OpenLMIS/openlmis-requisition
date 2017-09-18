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

package org.openlmis.requisition.utils;

import static java.util.stream.Collectors.toList;

import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
public class RequisitionExportHelper {

  @Autowired
  private OrderableReferenceDataService orderableReferenceDataService;

  /**
   * Return list of RequisitionLineItemDtos for a given RequisitionLineItem.
   *
   * @param requisitionLineItems List of RequisitionLineItems to be exported to Dto
   * @return list of RequisitionLineItemDtos
   */
  public List<RequisitionLineItemDto> exportToDtos(
      List<RequisitionLineItem> requisitionLineItems) {
    List<OrderableDto> orderables = orderableReferenceDataService.findByIds(
            requisitionLineItems.stream().map(RequisitionLineItem::getOrderableId)
                    .collect(Collectors.toSet()));
    return requisitionLineItems.stream().map(
        item -> exportToDto(item, orderables)).collect(toList());
  }

  private RequisitionLineItemDto exportToDto(RequisitionLineItem requisitionLineItem,
                                             List<OrderableDto> orderables) {
    RequisitionLineItemDto dto = new RequisitionLineItemDto();
    Optional<OrderableDto> orderableDto = orderables.stream().filter(
        orderable -> orderable.getId().equals(requisitionLineItem.getOrderableId())).findAny();
    requisitionLineItem.export(dto, orderableDto.orElse(null));
    return dto;
  }
}
