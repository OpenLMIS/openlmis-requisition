package org.openlmis.hierarchyandsupervision.repository;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.custom.UserRepositoryCustom;
import org.openlmis.referencedata.repository.ReferenceDataRepository;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface UserRepository extends
        ReferenceDataRepository<User, UUID>,
        UserRepositoryCustom {
  User findOneByUsername(@Param("username") String username);
}
