package org.openlmis.hierarchyandsupervision.repository;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.custom.UserRepositoryCustom;
import org.openlmis.referencedata.repository.ReferenceDataRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

@RepositoryRestResource(exported = true)
public interface UserRepository extends
        ReferenceDataRepository<User, UUID>,
        UserRepositoryCustom {

  @Override
  @RestResource
  <S extends User> S save(S entity);

  @Override
  @RestResource
  <S extends User> Iterable<S> save(Iterable<S> entities);

  User findOneByUsername(@Param("username") String username);
}
