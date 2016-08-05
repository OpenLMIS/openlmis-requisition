package org.openlmis.fulfillment.repository.custom;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;

import java.time.LocalDate;
import java.util.List;

public interface OrderRepositoryCustom {

    List<Order> searchOrders(Facility supplyingFacility, Facility requestingFacility,
                             Program program, Period period, Schedule schedule,
                             LocalDate startDate, LocalDate endDate);
}
