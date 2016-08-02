package org.openlmis.product.service;

import org.openlmis.product.domain.ProductCategory;
import org.springframework.stereotype.Service;

import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

@Service
public class ProductCategoryService {

  @PersistenceContext
  EntityManager entityManager;

  /**
   * Finds ProductCategories matching all of provided parameters.
   * @param code code of productCategory.
   * @return list of all ProductCategories matching all of provided parameters.
   */
  public List<ProductCategory> searchProductCategories( String code ) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<ProductCategory> query = builder.createQuery(ProductCategory.class);
    Root<ProductCategory> root = query.from(ProductCategory.class);
    Predicate predicate = builder.conjunction();
    if (code != null) {
      predicate = builder.and(
              predicate,
              builder.equal(
                      root.get("code"), code));
    }
    query.where(predicate);
    return entityManager.createQuery(query).getResultList();
  }
}
