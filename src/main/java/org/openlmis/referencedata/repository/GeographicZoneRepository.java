package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.GeographicZone;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface GeographicZoneRepository extends ReferenceDataRepository<GeographicZone, UUID> {

  @Override
  @RestResource
  <S extends GeographicZone> S save(S entity);

  @Override
  @RestResource
  <S extends GeographicZone> Iterable<S> save(Iterable<S> entities);
}
