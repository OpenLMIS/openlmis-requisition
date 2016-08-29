package org.openlmis.hierarchyandsupervision.repository;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.custom.UserRepositoryCustom;
import org.openlmis.referencedata.repository.ReferenceDataRepository;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface UserRepository extends
        ReferenceDataRepository<User, UUID>,
        UserRepositoryCustom {

  @Override
  <S extends User> S save(S entity);

  @Override
  <S extends User> Iterable<S> save(Iterable<S> entities);

  User findOneByUsername(@Param("username") String username);
}
