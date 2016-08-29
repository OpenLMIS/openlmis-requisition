package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.GeographicLevel;

import java.util.UUID;

public interface GeographicLevelRepository extends ReferenceDataRepository<GeographicLevel, UUID> {

  @Override
  <S extends GeographicLevel> S save(S entity);

  @Override
  <S extends GeographicLevel> Iterable<S> save(Iterable<S> entities);
}
