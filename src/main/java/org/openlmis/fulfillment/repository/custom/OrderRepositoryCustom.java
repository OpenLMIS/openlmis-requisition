package org.openlmis.fulfillment.repository.custom;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProgramDto;

import java.util.List;

public interface OrderRepositoryCustom {

  List<Order> searchOrders(FacilityDto supplyingFacility, FacilityDto requestingFacility,
                           ProgramDto program);
}
