package org.openlmis.fulfillment.repository.custom.impl;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.repository.custom.OrderRepositoryCustom;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.Program;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.util.List;

public class OrderRepositoryImpl implements OrderRepositoryCustom {

  @PersistenceContext
  private EntityManager entityManager;

  /**
   * Method returns all Orders with matched parameters.
   * @param supplyingFacility supplyingFacility of searched Orders.
   * @param requestingFacility requestingFacility of searched Orders.
   * @param program program of searched Orders.
   * @return List of Orders with matched parameters.
   */
  public List<Order> searchOrders(Facility supplyingFacility, Facility requestingFacility,
                                  Program program) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Order> query = builder.createQuery(Order.class);
    Root<Order> root = query.from(Order.class);
    Predicate predicate = builder.conjunction();
    if (supplyingFacility != null) {
      predicate = builder.and(
              predicate,
              builder.equal(
                      root.get("supplyingFacility"), supplyingFacility));
    }
    if (requestingFacility != null) {
      predicate = builder.and(
              predicate,
              builder.equal(
                      root.get("requestingFacility"), requestingFacility));
    }
    if (program != null) {
      predicate = builder.and(predicate,
              builder.equal(
                      root.get("program"), program));
    }
    query.where(predicate);
    return entityManager.createQuery(query).getResultList();
  }

}