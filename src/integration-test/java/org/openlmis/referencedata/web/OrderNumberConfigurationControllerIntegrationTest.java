package org.openlmis.referencedata.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.springframework.security.oauth2.common.OAuth2AccessToken.ACCESS_TOKEN;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Test;
import org.openlmis.fulfillment.domain.OrderNumberConfiguration;
import org.openlmis.fulfillment.repository.OrderNumberConfigurationRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

public class OrderNumberConfigurationControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/orderNumberConfigurations";

  @Autowired
  private OrderNumberConfigurationRepository orderNumberConfigurationRepository;

  @Test
  public void shouldUpdateOrderNumberConfiguration() {
    OrderNumberConfiguration orderNumberConfiguration = getOrderNumberConfiguration();
    orderNumberConfiguration.setOrderNumberPrefix("testPrefix");

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(orderNumberConfiguration)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(200);

    orderNumberConfiguration = getOrderNumberConfiguration();
    assertEquals("testPrefix", orderNumberConfiguration.getOrderNumberPrefix());

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private OrderNumberConfiguration getOrderNumberConfiguration() {
    return orderNumberConfigurationRepository.findAll().iterator().next();
  }
}