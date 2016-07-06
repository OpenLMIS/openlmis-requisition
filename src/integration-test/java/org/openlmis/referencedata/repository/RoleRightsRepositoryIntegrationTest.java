package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Role;
import org.springframework.beans.factory.annotation.Autowired;
/** Allow testing roleRightsRepository. */

public class RoleRightsRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<Role> {

  @Autowired
  RoleRightsRepository repository;

  RoleRightsRepository getRepository() {
    return this.repository;
  }

  @Override
  Role generateInstance() {
    int instanceNumber = this.getNextInstanceNumber();
    Role role = new Role();
    role.setName(String.valueOf(instanceNumber));
    return role;
  }
}
