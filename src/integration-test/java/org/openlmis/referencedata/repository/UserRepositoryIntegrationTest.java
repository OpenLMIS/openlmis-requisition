package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Test;
import org.openlmis.referencedata.domain.Role;
import org.openlmis.referencedata.domain.User;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class UserRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<User> {

  @Autowired
  UserRepository repository;

  @Autowired
  RoleRightsRepository roleRightsRepository;

  UserRepository getRepository() {
    return this.repository;
  }

  User generateInstance() {
    int instanceNumber = this.getNextInstanceNumber();
    User user = new User();
    user.setUsername("user" + instanceNumber);
    user.setPassword("test" + instanceNumber);
    user.setFirstName("Test");
    user.setLastName("User");
    return user;
  }

  @Test
  public void testRoleAssignment() {
    User user = this.generateInstance();
    Role role = new Role();
    role.setName("Test");
    user = repository.save(user);
    user = repository.findOne(user.getId());
    List<Role> roles = new ArrayList<Role>();
    roles.add(role);
    Assert.assertNotEquals(roles, user.getRoles());
    user.setRoles(roles);
    user = repository.save(user);
    user = repository.findOne(user.getId());
    Assert.assertEquals(roles, user.getRoles());
  }
}
