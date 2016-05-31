package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.User;
import org.springframework.data.repository.CrudRepository;

public interface UserRepository extends CrudRepository<User, Integer> {
}
