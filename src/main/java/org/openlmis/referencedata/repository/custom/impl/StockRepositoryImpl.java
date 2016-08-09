package org.openlmis.referencedata.repository.custom.impl;

import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.Stock;
import org.openlmis.referencedata.repository.custom.StockRepositoryCustom;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.util.List;

public class StockRepositoryImpl implements StockRepositoryCustom {

  @PersistenceContext
  private EntityManager entityManager;

  /**
   * Finds Stocks matching all of provided parameters.
   * @param product product of searched Stocks.
   * @return list of all Stocks matching all of provided parameters.
   */
  public List<Stock> searchStocks(
          Product product) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Stock> query = builder.createQuery(Stock.class);
    Root<Stock> root = query.from(Stock.class);
    Predicate predicate = builder.conjunction();
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
