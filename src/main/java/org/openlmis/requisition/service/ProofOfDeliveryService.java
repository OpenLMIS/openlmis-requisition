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
  private PeriodService periodService;

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
