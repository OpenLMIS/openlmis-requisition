package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.repository.GeographicLevelRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.util.Arrays;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;


public class GeographicLevelControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/geographicLevels";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String ACCESS_TOKEN = "access_token";
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");
  private static final String CODE = "OpenLMIS";

  @Autowired
  private GeographicLevelRepository geographicLevelRepository;

  private GeographicLevel geoLevel = new GeographicLevel();

  @Before
  public void setUp() {
    geoLevel.setCode("geoLevelCode");
    geoLevel.setLevelNumber(1);
    geographicLevelRepository.save(geoLevel);
  }

  @Test
  public void shouldDeleteGeographicLevel() {

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", geoLevel.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(204);

    assertFalse(geographicLevelRepository.exists(geoLevel.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateGeographicLevel() {

    geographicLevelRepository.delete(geoLevel);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(geoLevel)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateGeographicLevel() {

    geoLevel.setCode(CODE);

    GeographicLevel response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", geoLevel.getId())
          .body(geoLevel)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(GeographicLevel.class);

    assertEquals(response.getCode(), CODE);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewGeographicLevelIfDoesNotExists() {

    geographicLevelRepository.delete(geoLevel);
    geoLevel.setCode(CODE);

    GeographicLevel response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", ID)
          .body(geoLevel)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(GeographicLevel.class);

    assertEquals(response.getCode(), CODE);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllGeographicLevels() {

    GeographicLevel[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(GeographicLevel[].class);

    Iterable<GeographicLevel> geographicLevels = Arrays.asList(response);
    assertTrue(geographicLevels.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenGeographicLevel() {

    GeographicLevel response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", geoLevel.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(GeographicLevel.class);

    assertTrue(geographicLevelRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonexistentGeographicLevel() {

    geographicLevelRepository.delete(geoLevel);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", geoLevel.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
