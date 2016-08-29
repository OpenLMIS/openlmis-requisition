package org.openlmis.referencedata.web;


import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.FacilityType;
import org.openlmis.referencedata.repository.FacilityTypeRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.util.Arrays;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

public class FacilityTypeControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/facilityTypes";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String ACCESS_TOKEN = "access_token";

  @Autowired
  private FacilityTypeRepository facilityTypeRepository;

  private FacilityType facilityType = new FacilityType();

  @Before
  public void setUp() {
    facilityType.setCode("code");
    facilityTypeRepository.save(facilityType);
  }

  @Test
  public void shouldDeleteFacilityType() {

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", facilityType.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(204);

    assertFalse(facilityTypeRepository.exists(facilityType.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateFacilityType() {

    facilityTypeRepository.delete(facilityType);

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .body(facilityType)
          .when()
          .post(RESOURCE_URL)
          .then()
          .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateFacilityType() {

    facilityType.setDescription("OpenLMIS");

    FacilityType response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", facilityType.getId())
          .body(facilityType)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(FacilityType.class);

    assertEquals(response.getDescription(), "OpenLMIS");
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllFacilityTypes() {

    FacilityType[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(FacilityType[].class);

    Iterable<FacilityType> facilityTypes = Arrays.asList(response);
    assertTrue(facilityTypes.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenFacilityType() {

    FacilityType response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", facilityType.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(FacilityType.class);

    assertTrue(facilityTypeRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
