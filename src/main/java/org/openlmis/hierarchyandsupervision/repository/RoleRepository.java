package org.openlmis.hierarchyandsupervision.repository;

import org.openlmis.hierarchyandsupervision.domain.Role;
import org.openlmis.referencedata.repository.ReferenceDataRepository;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface RoleRepository extends ReferenceDataRepository<Role, UUID> {
  @Override
  @RestResource
  <S extends Role> S save(S entity);

  @Override
  @RestResource
  <S extends Role> Iterable<S> save(Iterable<S> entities);
}
