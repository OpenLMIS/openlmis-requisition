package org.openlmis.requisition.web;


import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.springframework.security.oauth2.common.OAuth2AccessToken.ACCESS_TOKEN;

import org.junit.Test;
import org.openlmis.fulfillment.domain.OrderNumberConfiguration;
import org.openlmis.fulfillment.repository.OrderNumberConfigurationRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import guru.nidi.ramltester.junit.RamlMatchers;

public class OrderNumberConfigurationControllerComponentTest extends BaseWebComponentTest {

  private static final String RESOURCE_URL = "/api/orderNumberConfigurations";

  @Autowired
  private OrderNumberConfigurationRepository orderNumberConfigurationRepository;

  @Test
  public void shouldUpdateOrderNumberConfiguration() {
    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration("prefix", true, true, true);
    orderNumberConfigurationRepository.deleteAll();

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(orderNumberConfiguration)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(200)
        .extract()
        .as(OrderNumberConfiguration.class);

    assertEquals(1, orderNumberConfigurationRepository.count());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

}