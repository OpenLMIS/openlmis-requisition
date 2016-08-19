package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.FacilityOperator;
import org.openlmis.referencedata.repository.FacilityOperatorRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;

public class FacilityOperatorControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String ACCESS_TOKEN = "access_token";
  private static final String RESOURCE_URL = "/api/facilityOperators";
  private static final String ID_URL = RESOURCE_URL + "/{id}";

  @Autowired
  private FacilityOperatorRepository facilityOperatorRepository;

  private List<FacilityOperator> facilityOperators;

  private Integer currentInstanceNumber;

  @Before
  public void setUp() {
    facilityOperators = new ArrayList<>();
    currentInstanceNumber = 0;
    for (int facilityOperatorCount = 0; facilityOperatorCount < 5; facilityOperatorCount++) {
      currentInstanceNumber = generateInstanceNumber();
      FacilityOperator facilityOperator = generateFacilityOperator();
      facilityOperators.add(facilityOperatorRepository.save(facilityOperator));
    }
  }

  @Test
  public void testShouldCreateFacilityOperator() {
    facilityOperatorRepository.delete(facilityOperators);
    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .body(facilityOperators.get(0))
            .when()
            .post(RESOURCE_URL)
            .then()
            .statusCode(201);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }


  @Test
  public void testShouldGetAllFacilityOperators() {
    FacilityOperator[] response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .when()
            .get(RESOURCE_URL)
            .then()
            .statusCode(200)
            .extract().as(FacilityOperator[].class);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
    assertEquals(5,response.length);
  }

  @Test
  public void testShouldUpdateFacilityOperator() {
    facilityOperators.get(0).setName("NewNameUpdate");
    FacilityOperator response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", facilityOperators.get(0).getId())
            .body(facilityOperators.get(0))
            .when()
            .put(ID_URL)
            .then()
            .statusCode(200)
            .extract().as(FacilityOperator.class);

    assertEquals(response.getName(), "NewNameUpdate");
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }


  @Test
  public void testShouldGetFacilityOperator() {

    FacilityOperator response = restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", facilityOperators.get(0).getId())
            .when()
            .get(ID_URL)
            .then()
            .statusCode(200)
            .extract().as(FacilityOperator.class);

    assertTrue(facilityOperatorRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldDeleteFacilityOperator() {

    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", facilityOperators.get(0).getId())
            .when()
            .delete(ID_URL)
            .then()
            .statusCode(204);

    assertFalse(facilityOperatorRepository.exists(facilityOperators.get(0).getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private FacilityOperator generateFacilityOperator() {
    FacilityOperator facilityOperator = new FacilityOperator();
    facilityOperator.setName("FacilityOperatorName" + currentInstanceNumber);
    facilityOperator.setCode("code" + currentInstanceNumber);
    facilityOperator.setDescription("description" + currentInstanceNumber);
    facilityOperator.setDisplayOrder(currentInstanceNumber);
    return  facilityOperator;
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber++;
    return currentInstanceNumber;
  }
}
