package org.openlmis.requisition.repository.custom.impl;

import org.openlmis.product.domain.Product;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.repository.custom.RequisitionLineRepositoryCustom;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.util.List;

public class RequisitionLineRepositoryImpl implements RequisitionLineRepositoryCustom {

  @PersistenceContext
  private EntityManager entityManager;

  /**
   * Returns all requisition lines with matched parameters.
   */
  public List<RequisitionLine> searchRequisitionLines(Requisition requisition, Product product) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<RequisitionLine> query = builder.createQuery(RequisitionLine.class);
    Root<RequisitionLine> root = query.from(RequisitionLine.class);
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
                      root.get("product"), product));
    }

    query.where(predicate);
    return entityManager.createQuery(query).getResultList();
  }
}
