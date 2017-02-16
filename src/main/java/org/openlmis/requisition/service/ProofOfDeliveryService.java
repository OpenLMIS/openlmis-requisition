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

package org.openlmis.requisition.service;

import static org.apache.commons.lang.BooleanUtils.isTrue;
import static org.springframework.util.CollectionUtils.isEmpty;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.dto.ProofOfDeliveryDto;
import org.openlmis.requisition.service.fulfillment.OrderFulfillmentService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Collection;

@Service
public class ProofOfDeliveryService {

  @Autowired
  private OrderFulfillmentService orderFulfillmentService;

  ProofOfDeliveryDto get(Requisition requisition) {
    if (RequisitionStatus.SKIPPED == requisition.getStatus()) {
      return null;
    }

    if (isTrue(requisition.getEmergency())) {
      return null;
    }

    Collection<OrderDto> orders = orderFulfillmentService.search(
        null, requisition.getFacilityId(), requisition.getProgramId(),
        requisition.getProcessingPeriodId(), null);

    if (isEmpty(orders)) {
      return null;
    }

    OrderDto order = orders.iterator().next();

    Collection<ProofOfDeliveryDto> proofOfDeliveries = orderFulfillmentService
        .getProofOfDeliveries(order.getId());

    return isEmpty(proofOfDeliveries) ? null : proofOfDeliveries.iterator().next();
  }

}
