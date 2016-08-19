package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.referencedata.domain.FacilityOperator;
import org.openlmis.referencedata.repository.FacilityOperatorRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import static org.junit.Assert.assertThat;

public class FacilityOperatorControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String ACCESS_TOKEN = "access_token";
  private static final String RESOURCE_URL = "/api/facilityOperators";

  @Autowired
  private FacilityOperatorRepository facilityOperatorRepository;

  private FacilityOperator facilityOperator;

  private Integer currentInstanceNumber;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    currentInstanceNumber = generateInstanceNumber();
    facilityOperator = generateFacilityOperator();
    facilityOperatorRepository.save(facilityOperator);
  }

  @Test
  public void testShouldCreateOrder() {
    facilityOperatorRepository.delete(facilityOperator);
    restAssured.given()
            .queryParam(ACCESS_TOKEN, getToken())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .body(facilityOperator)
            .when()
            .post(RESOURCE_URL)
            .then()
            .statusCode(201);

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
