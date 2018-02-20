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

package org.openlmis.requisition.domain.requisition;

import static org.apache.commons.collections4.CollectionUtils.isEmpty;
import static org.openlmis.requisition.domain.requisition.Requisition.REQUISITION_LINE_ITEMS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_ORDERABLE_NOT_IN_AVAILABLE_LIST;

import org.openlmis.requisition.domain.DomainValidator;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.openlmis.requisition.utils.Message;

import lombok.AllArgsConstructor;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@AllArgsConstructor
public class AvailableOrderableValidator implements DomainValidator {
  private OrderableReferenceDataService orderableReferenceDataService;

  private final Requisition requisitionUpdater;
  private final Requisition requisitionToUpdate;

  @Override
  public void validate(Map<String, Message> errors) {
    Set<UUID> currentOrderableIds = getOrderableIds(requisitionUpdater);

    Set<UUID> existingOrderableIds = getOrderableIds(requisitionToUpdate);
    existingOrderableIds.addAll(requisitionToUpdate.getAvailableProducts());

    currentOrderableIds.removeAll(existingOrderableIds);

    if (isEmpty(currentOrderableIds)) {
      // all orderables are present in the available list
      return;
    }

    List<OrderableDto> orderables = orderableReferenceDataService.findByIds(currentOrderableIds);
    String codes = orderables
        .stream()
        .map(OrderableDto::getProductCode)
        .collect(Collectors.joining(", "));

    errors.put(REQUISITION_LINE_ITEMS, new Message(ERROR_ORDERABLE_NOT_IN_AVAILABLE_LIST, codes));
  }

  private Set<UUID> getOrderableIds(Requisition requisition) {
    return requisition
        .getRequisitionLineItems()
        .stream()
        .map(RequisitionLineItem::getOrderableId)
        .collect(Collectors.toSet());
  }
}
