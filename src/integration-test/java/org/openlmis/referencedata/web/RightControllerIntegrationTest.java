package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.hierarchyandsupervision.domain.Right;
import org.openlmis.hierarchyandsupervision.repository.RightRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.util.Arrays;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

public class RightControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/rights";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String ACCESS_TOKEN = "access_token";
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");
  private static final String DESCRIPTION = "OpenLMIS";

  @Autowired
  private RightRepository rightRepository;

  private Right right = new Right();

  @Before
  public void setUp() {
    right.setName("rightName");
    right.setRightType("rightType");
    rightRepository.save(right);
  }

  @Test
  public void shouldDeleteRight() {

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", right.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(204);

    Assert.assertFalse(rightRepository.exists(right.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateRight() {

    rightRepository.delete(right);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(right)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateRight() {

    right.setDescription(DESCRIPTION);

    Right response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", right.getId())
          .body(right)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(Right.class);

    assertEquals(response.getDescription(), DESCRIPTION);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewRightIfDoesNotExists() {

    rightRepository.delete(right);
    right.setDescription(DESCRIPTION);

    Right response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", ID)
          .body(right)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(Right.class);

    assertEquals(response.getDescription(), DESCRIPTION);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllRights() {

    Right[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(Right[].class);

    Iterable<Right> rights = Arrays.asList(response);
    assertTrue(rights.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenRight() {

    Right response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", right.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(Right.class);

    assertTrue(rightRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonexistentRight() {

    rightRepository.delete(right);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", right.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
