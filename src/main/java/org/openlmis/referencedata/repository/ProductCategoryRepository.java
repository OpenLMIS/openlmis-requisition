package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.ProductCategory;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface ProductCategoryRepository extends PagingAndSortingRepository<ProductCategory, UUID> {

  ProductCategory findByCode(@Param("code") String code);
}
