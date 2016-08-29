package org.openlmis.hierarchyandsupervision.repository;

import org.openlmis.hierarchyandsupervision.domain.Right;
import org.openlmis.referencedata.repository.ReferenceDataRepository;

import java.util.UUID;

public interface RightRepository extends ReferenceDataRepository<Right, UUID> {
  @Override
  <S extends Right> S save(S entity);

  @Override
  <S extends Right> Iterable<S> save(Iterable<S> entities);
}
