package org.openlmis.hierarchyandsupervision.repository;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.referencedata.repository.ReferenceDataRepository;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

@RepositoryRestResource(exported = true)
public interface UserRepository extends ReferenceDataRepository<User, UUID> {

  @Override
  @RestResource
  <S extends User> S save(S entity);

  @Override
  @RestResource
  <S extends User> Iterable<S> save(Iterable<S> entities);
}
