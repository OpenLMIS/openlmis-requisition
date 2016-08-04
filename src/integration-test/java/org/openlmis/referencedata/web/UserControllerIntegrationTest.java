package org.openlmis.referencedata.web;

import static org.junit.Assert.assertThat;

import com.jayway.restassured.RestAssured;
import guru.nidi.ramltester.RamlDefinition;
import guru.nidi.ramltester.RamlLoaders;
import guru.nidi.ramltester.junit.RamlMatchers;
import guru.nidi.ramltester.restassured.RestAssuredClient;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.openlmis.referencedata.repository.GeographicLevelRepository;
import org.openlmis.referencedata.repository.GeographicZoneRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class UserControllerIntegrationTest extends BaseWebIntegrationTest {

  @Autowired
  private UserRepository userRepository;

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  @Autowired
  private GeographicZoneRepository geographicZoneRepository;

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  @Autowired
  private FacilityRepository facilityRepository;

  private static final String RESOURCE_URL = BASE_URL + "/api/users";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String ACCESS_TOKEN = "access_token";
  private static final String USERNAME = "username";
  private static final String FIRST_NAME = "firstName";
  private static final String LAST_NAME = "lastName";
  private static final String HOME_FACILITY = "homeFacility";
  private static final String ACTIVE = "active";
  private static final String VERIFIED = "verified";
  private static final String RAML_ASSERT_MESSAGE = "HTTP request/response should match RAML "
          + "definition.";

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
    facilityRepository.deleteAll();
    facilityTypeRepository.deleteAll();
    geographicZoneRepository.deleteAll();
    geographicLevelRepository.deleteAll();
  }

  @Test
  public void testSearchUsers() {
    User[] response = restAssured.given()
            .queryParam(USERNAME, users.get(0).getUsername())
            .queryParam(FIRST_NAME, users.get(0).getFirstName())
            .queryParam(LAST_NAME, users.get(0).getLastName())
            .queryParam(HOME_FACILITY, users.get(0).getHomeFacility().getId())
            .queryParam(ACTIVE, users.get(0).getActive())
            .queryParam(VERIFIED, users.get(0).getVerified())
            .queryParam(ACCESS_TOKEN, getToken())
            .when()
            .get(SEARCH_URL).as(User[].class);

    assertThat(RAML_ASSERT_MESSAGE , restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    Assert.assertEquals(1,response.length);
    for ( User user : response ) {
      Assert.assertEquals(
              user.getUsername(),
              users.get(0).getUsername());
      Assert.assertEquals(
              user.getFirstName(),
              users.get(0).getFirstName());
      Assert.assertEquals(
              user.getLastName(),
              users.get(0).getLastName());
      Assert.assertEquals(
              user.getHomeFacility().getId(),
              users.get(0).getHomeFacility().getId());
      Assert.assertEquals(
              user.getActive(),
              users.get(0).getActive());
      Assert.assertEquals(
              user.getVerified(),
              users.get(0).getVerified());
    }
  }

  private User generateUser() {
    User user = new User();
    Integer instanceNumber = generateInstanceNumber();
    user.setFirstName("Ala" + instanceNumber);
    user.setLastName("ma" + instanceNumber);
    user.setUsername("kota" + instanceNumber);
    user.setPassword("iDobrze" + instanceNumber);
    user.setHomeFacility(generateFacility());
    user.setVerified(true);
    user.setActive(true);
    userRepository.save(user);
    return user;
  }

  private Facility generateFacility() {
    Integer instanceNumber = + generateInstanceNumber();
    GeographicLevel geographicLevel = generateGeographicLevel();
    GeographicZone geographicZone = generateGeographicZone(geographicLevel);
    FacilityType facilityType = generateFacilityType();
    Facility facility = new Facility();
    facility.setType(facilityType);
    facility.setGeographicZone(geographicZone);
    facility.setCode("FacilityCode" + instanceNumber);
    facility.setName("FacilityName" + instanceNumber);
    facility.setDescription("FacilityDescription" + instanceNumber);
    facility.setActive(true);
    facility.setEnabled(true);
    facility.setStockInventory(null);
    facilityRepository.save(facility);
    return facility;
  }

  private GeographicLevel generateGeographicLevel() {
    GeographicLevel geographicLevel = new GeographicLevel();
    geographicLevel.setCode("GeographicLevel" + generateInstanceNumber());
    geographicLevel.setLevelNumber(1);
    geographicLevelRepository.save(geographicLevel);
    return geographicLevel;
  }

  private GeographicZone generateGeographicZone(GeographicLevel geographicLevel) {
    GeographicZone geographicZone = new GeographicZone();
    geographicZone.setCode("GeographicZone" + generateInstanceNumber());
    geographicZone.setLevel(geographicLevel);
    geographicZoneRepository.save(geographicZone);
    return geographicZone;
  }

  private FacilityType generateFacilityType() {
    FacilityType facilityType = new FacilityType();
    facilityType.setCode("FacilityType" + generateInstanceNumber());
    facilityTypeRepository.save(facilityType);
    return facilityType;
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }
}
