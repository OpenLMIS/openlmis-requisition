package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.FacilityType;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface FacilityTypeRepository extends ReferenceDataRepository<FacilityType, UUID> {

  @Override
  @RestResource
  <S extends FacilityType> S save(S entity);

  @Override
  @RestResource
  <S extends FacilityType> Iterable<S> save(Iterable<S> entities);
}
