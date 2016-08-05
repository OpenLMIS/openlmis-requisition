package org.openlmis.referencedata.service;

import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.domain.Stock;
import org.openlmis.referencedata.domain.StockInventory;
import org.springframework.stereotype.Service;

import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

@Service
public class StockService {

  @PersistenceContext
  private EntityManager entityManager;

  /**
   * Finds Stocks matching all of provided parameters.
   * @param stockInventory stockInventory of searched ProgramProducts.
   * @param product product of searched ProgramProducts.
   * @return list of all Stocks matching all of provided parameters.
   */
  public List<Stock> searchStocks(
          StockInventory stockInventory,
          Product product) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Stock> query = builder.createQuery(Stock.class);
    Root<Stock> root = query.from(Stock.class);
    Predicate predicate = builder.conjunction();
    if (stockInventory != null) {
      predicate = builder.and(
              predicate,
              builder.equal(
                      root.get("stockInventory"), stockInventory));
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
