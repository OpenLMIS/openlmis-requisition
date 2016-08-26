package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Facility;

import java.util.UUID;

public interface FacilityRepository extends ReferenceDataRepository<Facility, UUID> {

  @Override
  <S extends Facility> S save(S entity);

  @Override
  <S extends Facility> Iterable<S> save(Iterable<S> entities);
}
