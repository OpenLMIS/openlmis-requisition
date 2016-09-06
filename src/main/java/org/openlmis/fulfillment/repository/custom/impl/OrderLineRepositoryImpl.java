package org.openlmis.fulfillment.repository.custom.impl;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLine;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

public class OrderLineRepositoryImpl {

  @PersistenceContext
  private EntityManager entityManager;

  /**
   * Method deletes given order line.
   * @param entity entity to be deleted.
   */
  public void delete(OrderLine entity) {
    Order order = entity.getOrder();
    order.getOrderLines().remove(entity);
    entityManager.merge(order);
  }
}
