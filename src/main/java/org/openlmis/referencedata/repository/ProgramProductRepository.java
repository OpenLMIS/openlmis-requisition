package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.ProgramProduct;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface ProgramProductRepository extends PagingAndSortingRepository<ProgramProduct, UUID> {

  @Override
  @RestResource
  <S extends ProgramProduct> S save(S entity);

  @Override
  @RestResource
  <S extends ProgramProduct> Iterable<S> save(Iterable<S> entities);
}
