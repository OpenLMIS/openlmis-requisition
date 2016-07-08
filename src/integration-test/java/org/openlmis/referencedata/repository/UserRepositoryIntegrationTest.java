package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Test;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.hierarchyandsupervision.domain.Role;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class UserRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<User> {

  @Autowired
  UserRepository repository;

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
  public void testMultipleRoleAssignment() {
    User user = this.generateInstance();
    Role role = new Role();
    role.setName("Test1");
    user = repository.save(user);
    List<Role> roles = new ArrayList<Role>();
    roles.add(role);
    role = new Role();
    role.setName("Test2");
    roles.add(role);
    Assert.assertNotEquals(roles, user.getRoles());
    user.setRoles(roles);
    user = repository.save(user);
    Assert.assertEquals(roles, user.getRoles());
  }

  @Test
  public void testMultipleUsersRoleAssignment() {
    User user1 = this.generateInstance();
    Role role = new Role();
    role.setName("Test");
    List<Role> roles = new ArrayList<Role>();
    roles.add(role);
    user1.setRoles(roles);
    user1 = repository.save(user1);
    User user2 = this.generateInstance();
    Assert.assertNotEquals(user1.getRoles(), user2.getRoles());
    user2.setRoles(user1.getRoles());
    user2 = repository.save(user2);
    Assert.assertEquals(user1.getRoles(), user2.getRoles());
  }
}
