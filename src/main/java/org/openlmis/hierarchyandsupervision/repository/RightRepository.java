package org.openlmis.hierarchyandsupervision.repository;

import org.openlmis.hierarchyandsupervision.domain.Right;
import org.openlmis.referencedata.repository.ReferenceDataRepository;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface RightRepository extends ReferenceDataRepository<Right, UUID> {
  @Override
  @RestResource
  <S extends Right> S save(S entity);

  @Override
  @RestResource
  <S extends Right> Iterable<S> save(Iterable<S> entities);
}
