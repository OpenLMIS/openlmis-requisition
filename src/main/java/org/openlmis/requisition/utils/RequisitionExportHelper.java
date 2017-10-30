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

import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.dto.BasicOrderableDto;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.slf4j.ext.XLogger;
import org.slf4j.ext.XLoggerFactory;
import org.slf4j.profiler.Profiler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Component
public class RequisitionExportHelper {

  private static final XLogger XLOGGER = XLoggerFactory.getXLogger(RequisitionExportHelper.class);

  @Autowired
  private OrderableReferenceDataService orderableReferenceDataService;

  /**
   * Return list of RequisitionLineItemDtos for a given RequisitionLineItem.
   *
   * @param requisitionLineItems List of RequisitionLineItems to be exported to Dto
   * @return list of RequisitionLineItemDtos
   */
  public List<RequisitionLineItemDto> exportToDtos(List<RequisitionLineItem> requisitionLineItems) {
    return exportToDtos(requisitionLineItems, null, false);
  }

  /**
   * Return list of RequisitionLineItemDtos for a given RequisitionLineItem.
   *
   * @param requisitionLineItems List of RequisitionLineItems to be exported to Dto
   * @param orderables Map of Orderables by id
   * @return list of RequisitionLineItemDtos
   */
  public List<RequisitionLineItemDto> exportToDtos(List<RequisitionLineItem> requisitionLineItems,
                                                   Map<UUID, OrderableDto> orderables,
                                                   boolean batch) {
    XLOGGER.entry(requisitionLineItems);
    Profiler profiler = new Profiler("EXPORT_LINE_ITEMS_TO_DTOS");
    profiler.setLogger(XLOGGER);

    Map<UUID, OrderableDto> orderablesForLines;
    if (orderables == null) {
      profiler.start("GET_ORDERABLE_IDS_FROM_LINE_ITEMS");
      Set<UUID> orderableIds = new HashSet<>(requisitionLineItems.size());
      for (RequisitionLineItem lineItem : requisitionLineItems) {
        orderableIds.add(lineItem.getOrderableId());
      }

      profiler.start("FIND_ORDERABLES_BY_IDS");
      orderablesForLines =
          orderableReferenceDataService.findByIds(orderableIds)
              .stream()
              .collect(Collectors.toMap(BasicOrderableDto::getId, orderable -> orderable));

    } else {
      orderablesForLines = orderables;
    }

    profiler.start("CONVERT_LINE_ITEMS_TO_DTOS");
    List<RequisitionLineItemDto> requisitionLineItemDtos =
        new ArrayList<>(requisitionLineItems.size());
    for (RequisitionLineItem lineItem : requisitionLineItems) {
      requisitionLineItemDtos.add(exportToDto(lineItem, orderablesForLines, batch));
    }

    profiler.stop().log();
    XLOGGER.exit(requisitionLineItemDtos);
    return requisitionLineItemDtos;
  }

  private RequisitionLineItemDto exportToDto(RequisitionLineItem requisitionLineItem,
                                             Map<UUID, OrderableDto> orderables,
                                             boolean batch) {
    XLOGGER.entry(requisitionLineItem, orderables);
    Profiler profiler = new Profiler("EXPORT_LINE_ITEM_TO_DTO");
    profiler.setLogger(XLOGGER);

    profiler.start("CONSTRUCT_REQUISITION_LINE_ITEM_DTO");
    RequisitionLineItemDto dto = new RequisitionLineItemDto();

    profiler.start("GET_LINE_ITEM_ORDERABLE_FROM_ORDERABLES");
    OrderableDto orderableDto = orderables.get(requisitionLineItem.getOrderableId());

    profiler.start("EXPORT_TO_DTO");
    if (batch) {
      requisitionLineItem.basicExport(dto, orderableDto);
    } else {
      requisitionLineItem.export(dto, orderableDto);
    }


    profiler.stop().log();
    XLOGGER.exit(dto);
    return dto;
  }
}
