package org.openlmis.referencedata.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.openlmis.hierarchyandsupervision.utils.AuthUserRequest;
import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.openlmis.referencedata.repository.FacilityRepository;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.openlmis.referencedata.repository.GeographicLevelRepository;
import org.openlmis.referencedata.repository.GeographicZoneRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@SuppressWarnings("PMD.TooManyMethods")
public class UserControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/users";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String ACCESS_TOKEN = "access_token";
  private static final String USERNAME = "username";
  private static final String FIRST_NAME = "firstName";
  private static final String LAST_NAME = "lastName";
  private static final String HOME_FACILITY = "homeFacility";
  private static final String ACTIVE = "active";
  private static final String VERIFIED = "verified";

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

  private List<User> users;

  private static Integer currentInstanceNumber = 0;

  @Before
  public void setUp() {
    users = new ArrayList<>();
    for ( int userCount = 0; userCount < 5; userCount++ ) {
      users.add(createUser());
    }
  }

  @Test
  public void shouldFindUsers() {
    User[] response = restAssured.given()
            .queryParam(USERNAME, users.get(0).getUsername())
            .queryParam(FIRST_NAME, users.get(0).getFirstName())
            .queryParam(LAST_NAME, users.get(0).getLastName())
            .queryParam(HOME_FACILITY, users.get(0).getHomeFacility().getId())
            .queryParam(ACTIVE, users.get(0).getActive())
            .queryParam(VERIFIED, users.get(0).getVerified())
            .queryParam(ACCESS_TOKEN, getToken())
            .when()
            .get(SEARCH_URL)
            .then()
            .statusCode(200)
            .extract().as(User[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertEquals(1, response.length);
    for ( User user : response ) {
      assertEquals(
              user.getUsername(),
              users.get(0).getUsername());
      assertEquals(
              user.getFirstName(),
              users.get(0).getFirstName());
      assertEquals(
              user.getLastName(),
              users.get(0).getLastName());
      assertEquals(
              user.getHomeFacility().getId(),
              users.get(0).getHomeFacility().getId());
      assertEquals(
              user.getActive(),
              users.get(0).getActive());
      assertEquals(
              user.getVerified(),
              users.get(0).getVerified());
    }
  }

  @Test
  public void shouldCreateRequisitionAndAuthUsers() {
    User user = generateUser();

    User response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(user)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(200)
        .extract().as(User.class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertNotNull(response);

    User savedUser = userRepository.findOne(response.getId());
    assertNotNull(savedUser);

    assertEquals(user.getUsername(), savedUser.getUsername());
    assertEquals(user.getFirstName(), savedUser.getFirstName());
    assertEquals(user.getLastName(), savedUser.getLastName());
    assertEquals(user.getEmail(), savedUser.getEmail());
    assertEquals(user.getHomeFacility().getId(), savedUser.getHomeFacility().getId());
    assertEquals(user.getActive(), savedUser.getActive());
    assertEquals(user.getVerified(), savedUser.getVerified());

    AuthUserRequest authUser = getAutUserByUsername(savedUser.getUsername());
    assertNotNull(authUser);

    assertEquals(savedUser.getEmail(), authUser.getEmail());
    assertEquals(savedUser.getId(), authUser.getReferenceDataUserId());

    removeAuthUserByUsername(authUser.getUsername());
  }

  @Test
  public void shouldUpdateRequisitionAndAuthUsers() {
    User user = generateUser();

    user = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(user)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(200)
        .extract().as(User.class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertNotNull(user);

    User savedUser = userRepository.findOne(user.getId());
    assertNotNull(savedUser);

    user.setEmail(generateInstanceNumber() + "@mail.com");
    assertNotEquals(user.getEmail(), savedUser.getEmail());

    AuthUserRequest authUser = getAutUserByUsername(savedUser.getUsername());
    assertNotNull(authUser);

    assertEquals(savedUser.getEmail(), authUser.getEmail());
    assertEquals(savedUser.getId(), authUser.getReferenceDataUserId());

    User response = restAssured.given()
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(user)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(200)
        .extract().as(User.class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertNotNull(response);

    savedUser = userRepository.findOne(user.getId());
    assertNotNull(savedUser);

    assertEquals(user.getUsername(), savedUser.getUsername());
    assertEquals(user.getFirstName(), savedUser.getFirstName());
    assertEquals(user.getLastName(), savedUser.getLastName());
    assertEquals(user.getEmail(), savedUser.getEmail());
    assertEquals(user.getHomeFacility().getId(), savedUser.getHomeFacility().getId());
    assertEquals(user.getActive(), savedUser.getActive());
    assertEquals(user.getVerified(), savedUser.getVerified());

    authUser = getAutUserByUsername(savedUser.getUsername());
    assertNotNull(authUser);

    assertEquals(savedUser.getEmail(), authUser.getEmail());
    assertEquals(savedUser.getId(), authUser.getReferenceDataUserId());

    removeAuthUserByUsername(authUser.getUsername());
  }

  private AuthUserRequest getAutUserByUsername(String username) {
    String url = "http://auth:8080/api/users/search/findOneByUsername?username=" + username
        + "&access_token=" + getToken();

    RestTemplate restTemplate = new RestTemplate();
    return restTemplate.getForObject(url, AuthUserRequest.class);
  }

  private void removeAuthUserByUsername(String username) {
    String url = "http://auth:8080/api/users/search/findOneByUsername?username=" + username
        + "&access_token=" + getToken();

    RestTemplate restTemplate = new RestTemplate();
    Map map = restTemplate.getForObject(url, Map.class);
    String href = ((String) ((Map) ((Map) map.get("_links")).get("self")).get("href"));
    String id = href.split("users/")[1];

    url = "http://auth:8080/api/users/" + id + "?access_token=" + getToken();
    restTemplate.delete(url);
  }

  private User createUser() {
    return userRepository.save(generateUser());
  }

  private User generateUser() {
    User user = new User();
    Integer instanceNumber = generateInstanceNumber();
    user.setFirstName("Ala" + instanceNumber);
    user.setLastName("ma" + instanceNumber);
    user.setUsername("kota" + instanceNumber);
    user.setEmail(instanceNumber + "@mail.com");
    user.setTimezone("UTC");
    user.setHomeFacility(generateFacility());
    user.setVerified(true);
    user.setActive(true);
    user.setRestrictLogin(false);
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
