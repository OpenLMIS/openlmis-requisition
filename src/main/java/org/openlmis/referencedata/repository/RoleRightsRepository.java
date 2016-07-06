package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Role;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface RoleRightsRepository extends ReferenceDataRepository<Role, UUID> {
  //Accessible via http://127.0.0.1:8080/api/roles/search/findByName?name={name}
  Role findByName(@Param("name") String name);
}
