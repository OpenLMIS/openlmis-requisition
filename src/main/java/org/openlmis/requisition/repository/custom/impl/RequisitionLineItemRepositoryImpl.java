package org.openlmis.requisition.repository.custom.impl;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.repository.custom.RequisitionLineItemRepositoryCustom;

import java.util.List;
import java.util.UUID;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

public class RequisitionLineItemRepositoryImpl implements RequisitionLineItemRepositoryCustom {

  @PersistenceContext
  private EntityManager entityManager;

  /**
   * Method returns all Requisition line items with matched parameters.
   * @param requisition requisition of searched requisition line items.
   * @param product product of searched requisition line items.
   * @return list of requisition line items with matched parameters.
   */
  public List<RequisitionLineItem> searchRequisitionLineItems(Requisition requisition,
                                                              UUID product) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<RequisitionLineItem> query = builder.createQuery(RequisitionLineItem.class);
    Root<RequisitionLineItem> root = query.from(RequisitionLineItem.class);
    Predicate predicate = builder.conjunction();

    if (requisition != null) {
      predicate = builder.and(
              predicate,
              builder.equal(
                      root.get("requisition"), requisition));
    }
    if (product != null) {
      predicate = builder.and(
              predicate,
              builder.equal(
                      root.get("orderableProduct"), product));
    }

    query.where(predicate);
    return entityManager.createQuery(query).getResultList();
  }

  /**
   * Method deletes given requisition line item.
   * @param entity entity to be deleted.
   */
  public void delete(RequisitionLineItem entity) {
    Requisition requisition = entity.getRequisition();
    requisition.getRequisitionLineItems().remove(entity);
    entityManager.merge(requisition);
  }
}
