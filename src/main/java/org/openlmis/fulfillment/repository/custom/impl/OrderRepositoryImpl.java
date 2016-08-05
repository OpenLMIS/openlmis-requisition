package org.openlmis.fulfillment.repository.custom.impl;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.repository.custom.OrderRepositoryCustom;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Period;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.Schedule;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class OrderRepositoryImpl implements OrderRepositoryCustom {

    @PersistenceContext
    private EntityManager em;

    @Override
    public List<Order> searchOrders(Facility supplyingFacility,
                                    Facility requestingFacility,
                                    Program program,
                                    Period period, Schedule schedule,
                                    LocalDate startDate, LocalDate endDate) {
        String hqlQuery = "select o from Order as o, Requisition as r, Period as p "
                + "where o.supplyingFacility = :supplyingFacility";
        Map<String, Object> params = new HashMap<>();
        params.put("supplyingFacility", supplyingFacility);
        if (requestingFacility != null) {
            hqlQuery += " and o.requestingFacility = :requestingFacility";
            params.put("requestingFacility", requestingFacility);
        }
        if (program != null) {
            hqlQuery += " and o.program = :program";
            params.put("program", program);
        }
        if (period != null) {
            hqlQuery += " and r.processingPeriod = :period";
            params.put("period", period);
        }
        if (schedule != null) {
            hqlQuery += " and p.processingSchedule = :schedule";
            params.put("schedule", schedule);
        }
        if (startDate != null) {
            hqlQuery += " and p.startDate = :startDate";
            params.put("startDate", startDate);
        }
        if (endDate != null) {
            hqlQuery += " and p.endDate = :endDate";
            params.put("endDate", endDate);
        }
        Query query = em.createQuery(hqlQuery);

        for (String name : params.keySet()) {
            Object value = params.get(name);
            query.setParameter(name, value);
        }

        return query.getResultList();
    }
}
