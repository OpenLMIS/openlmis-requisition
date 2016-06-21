package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.GeographicLevel;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface GeographicLevelRepository extends ReferenceDataRepository<GeographicLevel, UUID> {

  @Override
  @RestResource
  <S extends GeographicLevel> S save(S entity);

  @Override
  @RestResource
  <S extends GeographicLevel> Iterable<S> save(Iterable<S> entities);
}
