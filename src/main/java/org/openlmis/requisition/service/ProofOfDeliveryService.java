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

import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.dto.ProofOfDeliveryDto;
import org.openlmis.requisition.dto.ShipmentDto;
import org.openlmis.requisition.service.fulfillment.OrderFulfillmentService;
import org.openlmis.requisition.service.fulfillment.ProofOfDeliveryFulfillmentService;
import org.openlmis.requisition.service.fulfillment.ShipmentFulfillmentService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class ProofOfDeliveryService {

  @Autowired
  private OrderFulfillmentService orderFulfillmentService;

  @Autowired
  private ShipmentFulfillmentService shipmentFulfillmentService;

  @Autowired
  private ProofOfDeliveryFulfillmentService proofOfDeliveryFulfillmentService;


  ProofOfDeliveryDto get(Requisition requisition) {
    if (RequisitionStatus.SKIPPED == requisition.getStatus()) {
      return null;
    }

    if (isTrue(requisition.getEmergency())) {
      return null;
    }

    List<OrderDto> orders = orderFulfillmentService.search(
        null, requisition.getFacilityId(), requisition.getProgramId(),
        requisition.getProcessingPeriodId(), null);

    if (isEmpty(orders)) {
      return null;
    }

    List<ShipmentDto> shipments = shipmentFulfillmentService.getShipments(orders.get(0).getId());

    if (isEmpty(shipments)) {
      return null;
    }

    List<ProofOfDeliveryDto> pods = proofOfDeliveryFulfillmentService
        .getProofOfDeliveries(shipments.get(0).getId());

    return isEmpty(pods) ? null : pods.get(0);
  }

}
