package org.openlmis.referencedata.service;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.openlmis.Application;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.hierarchyandsupervision.service.UserService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
public class UserServiceTest {

  @Autowired
  private UserService userService;

  @Autowired
  private UserRepository userRepository;

  private List<User> users;

  private Integer currentInstanceNumber;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    users = new ArrayList<>();
    users.add(generateUser());
  }

  @After
  public void cleanup() {
    Iterable<User> users = userService.searchUsers("kota1", null, null, null, null, null);
    if (users != null && users.iterator().hasNext()) {
      userRepository.delete(users);
    }
  }

  @Transactional
  @Test
  public void testSearchUsers() {
    List<User> receivedUsers = userService.searchUsers(
            users.get(0).getUsername(),
            null,
            null,
            null,
            null,
            null);
    Assert.assertEquals(1,receivedUsers.size());
    for ( User user : receivedUsers ) {
      Assert.assertEquals(user.getUsername(),users.get(0).getUsername());
    }
  }

  private User generateUser() {
    User user = new User();
    int instanceNumber = generateInstanceNumber();
    user.setFirstName("Ala" + instanceNumber);
    user.setLastName("ma" + instanceNumber);
    user.setUsername("kota" + instanceNumber);
    user.setPassword("iDobrze" + instanceNumber);
    user.setVerified(true);
    user.setActive(true);
    userRepository.save(user);
    return user;
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }
}
