package org.openlmis.referencedata.repository;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;

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
}
