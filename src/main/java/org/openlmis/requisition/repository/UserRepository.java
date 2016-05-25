package org.openlmis.requisition.repository;

import org.openlmis.requisition.domain.User;
import org.springframework.data.repository.CrudRepository;

public interface UserRepository extends CrudRepository<User, Integer> {
}
