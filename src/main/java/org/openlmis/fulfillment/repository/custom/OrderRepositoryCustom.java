package org.openlmis.fulfillment.repository.custom;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Program;

import java.util.List;

public interface OrderRepositoryCustom {

  List<Order> searchOrders(Facility supplyingFacility,
                           Facility requestingFacility,
                           Program program);
}