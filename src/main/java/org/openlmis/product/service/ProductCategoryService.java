package org.openlmis.product.service;

import org.openlmis.product.domain.ProductCategory;
import org.openlmis.product.repository.ProductCategoryRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class ProductCategoryService {

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  /**
   * Finds ProductCategories matching all of provided parameters.
   * @param code code of productCategory.
   * @return list of all ProductCategories matching all of provided parameters.
   */
  public List<ProductCategory> searchProductCategories( String code ) {
    return productCategoryRepository.searchProductCategories(code);
  }
}
