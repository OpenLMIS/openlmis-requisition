package org.openlmis.product.repository.custom;

import org.openlmis.product.domain.ProductCategory;

import java.util.List;

public interface ProductCategoryRepositoryCustom {

  List<ProductCategory> searchProductCategories(String code);
}
