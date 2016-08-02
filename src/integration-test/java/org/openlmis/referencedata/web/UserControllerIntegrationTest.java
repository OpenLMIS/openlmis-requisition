package org.openlmis.referencedata.web;

import com.jayway.restassured.RestAssured;
import guru.nidi.ramltester.RamlDefinition;
import guru.nidi.ramltester.RamlLoaders;
import guru.nidi.ramltester.restassured.RestAssuredClient;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class UserControllerIntegrationTest extends BaseWebIntegrationTest {

  @Autowired
  private UserRepository userRepository;

  private static final String RESOURCE_URL = BASE_URL + "/api/users";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String ACCESS_TOKEN = "access_token";
  private static final String USERNAME = "username";
  private static final String FIRSTNAME = "firstname";
  //private static final String LASTNAME = "lastName";
  //private static final String HOMEFACILITY = "homeFacility";
  //private static final String ACTIVE = "active";
  //private static final String VERIFIED = "verified";

  private List<User> users;

  private Integer currentInstanceNumber;
  private RamlDefinition ramlDefinition;
  private RestAssuredClient restAssured;

  @Before
  public void setUp() {
    users = new ArrayList<>();
    currentInstanceNumber = 0;
    for ( int userCount = 0; userCount < 5; userCount++ ) {
      users.add(generateUser());
    }
    RestAssured.baseURI = BASE_URL;
    ramlDefinition = RamlLoaders.fromClasspath().load("api-definition-raml.yaml");
    restAssured = ramlDefinition.createRestAssured();
  }

  @After
  public void cleanUp() {
    userRepository.delete(users);
  }

  @Test
  public void testSearchUsers() {
    User[] response = restAssured.given()
            .queryParam(USERNAME, users.get(0).getUsername())
            .queryParam(FIRSTNAME, users.get(0).getFirstName())
            .queryParam(ACCESS_TOKEN, getToken())
            .when()
            .get(SEARCH_URL).as(User[].class);

    Assert.assertEquals(1,response.length);
    for ( User order : response ) {
      Assert.assertEquals(
              order.getUsername(),
              users.get(0).getUsername());
      Assert.assertEquals(
              order.getFirstName(),
              users.get(0).getFirstName());
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
