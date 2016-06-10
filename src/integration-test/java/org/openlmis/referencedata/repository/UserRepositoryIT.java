package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.User;
import org.springframework.beans.factory.annotation.Autowired;

public class UserRepositoryIT extends BaseCrudRepositoryIT<User> {

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
