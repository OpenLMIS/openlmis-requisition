package org.openlmis.product.repository;

import org.openlmis.product.domain.ProductCategory;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface ProductCategoryRepository extends PagingAndSortingRepository<ProductCategory, UUID> {

  ProductCategory findByCode(@Param("code") String code);
}
