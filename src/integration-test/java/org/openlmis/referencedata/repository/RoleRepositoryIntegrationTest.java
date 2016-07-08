package org.openlmis.referencedata.repository;

import org.junit.Assert;
import org.junit.Test;
import org.openlmis.hierarchyandsupervision.domain.Right;
import org.openlmis.hierarchyandsupervision.domain.Role;
import org.openlmis.hierarchyandsupervision.repository.RoleRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

/** Allow testing roleRightsRepository. */

public class RoleRepositoryIntegrationTest extends BaseCrudRepositoryIntegrationTest<Role> {

  @Autowired
  RoleRepository repository;

  RoleRepository getRepository() {
    return this.repository;
  }

  @Override
  Role generateInstance() {
    int instanceNumber = this.getNextInstanceNumber();
    Role role = new Role();
    role.setName(String.valueOf(instanceNumber));
    return role;
  }

  @Test
  public void testMultipleRightAssignment() {
    Role role = this.generateInstance();
    Right right = new Right();
    right.setName("Test1");
    right.setRightType("Test1");
    role = repository.save(role);
    List<Right> rights = new ArrayList<Right>();
    rights.add(right);
    right = new Right();
    right.setName("Test2");
    right.setRightType("Test2");
    rights.add(right);
    Assert.assertNotEquals(rights, role.getRights());
    role.setRights(rights);
    role = repository.save(role);
    Assert.assertEquals(rights, role.getRights());
  }

  @Test
  public void testMultipleRolesRightAssignment() {
    Role role1 = this.generateInstance();
    Right right = new Right();
    right.setName("Test");
    List<Right> rights = new ArrayList<Right>();
    rights.add(right);
    role1.setRights(rights);
    role1 = repository.save(role1);
    Role role2 = this.generateInstance();
    Assert.assertNotEquals(role1.getRights(), role2.getRights());
    role2.setRights(role1.getRights());
    role2 = repository.save(role2);
    Assert.assertEquals(role1.getRights(), role2.getRights());
  }
}
