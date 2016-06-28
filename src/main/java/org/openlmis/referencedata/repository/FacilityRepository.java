package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Facility;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface FacilityRepository extends ReferenceDataRepository<Facility, UUID> {

  @Override
  @RestResource
  <S extends Facility> S save(S entity);

  @Override
  @RestResource
  <S extends Facility> Iterable<S> save(Iterable<S> entities);
}
