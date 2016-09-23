package org.openlmis.fulfillment.repository.custom.impl;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderLineItem;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

public class OrderLineItemRepositoryImpl {

  @PersistenceContext
  private EntityManager entityManager;

  /**
   * Method deletes given order line item.
   * @param entity entity to be deleted.
   */
  public void delete(OrderLineItem entity) {
    Order order = entity.getOrder();
    order.getOrderLineItems().remove(entity);
    entityManager.merge(order);
  }
}
