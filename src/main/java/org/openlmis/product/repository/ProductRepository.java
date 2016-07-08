package org.openlmis.product.repository;

import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.repository.ReferenceDataRepository;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface ProductRepository extends ReferenceDataRepository<Product, UUID> {
    //Add custom Product related members here. See UserRepository.java for examples.

  @Override
  @RestResource
  <S extends Product> S save (S entity);

  @Override
  @RestResource
  <S extends Product> Iterable<S> save (Iterable<S> entities);
}
