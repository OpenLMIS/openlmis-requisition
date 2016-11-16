package org.openlmis.requisition.web;


import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.springframework.security.oauth2.common.OAuth2AccessToken.ACCESS_TOKEN;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.domain.OrderNumberConfiguration;
import org.openlmis.fulfillment.dto.ConvertToOrderDto;
import org.openlmis.fulfillment.repository.OrderNumberConfigurationRepository;
import org.openlmis.fulfillment.repository.OrderRepository;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import guru.nidi.ramltester.junit.RamlMatchers;

import java.util.Collections;
import java.util.UUID;

public class OrderNumberConfigurationControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/orderNumberConfigurations";

  @Autowired
  private OrderNumberConfigurationRepository orderNumberConfigurationRepository;

  @Autowired
  private OrderRepository orderRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;

  private Requisition requisition;
  private ProgramDto programDto;
  private UUID facility = UUID.fromString("1d5bdd9c-8702-11e6-ae22-56b6b6499611");

  @Before
  public void setUp() {

    programDto = new ProgramDto();
    programDto.setId(UUID.fromString("35316636-6264-6331-2d34-3933322d3462"));
    programDto.setCode("code");

    requisition = new Requisition();
    requisition.setEmergency(true);
    requisition.setStatus(RequisitionStatus.APPROVED);
    requisition.setProgramId(programDto.getId());
    requisition.setFacilityId(facility);
    requisition.setProcessingPeriodId(UUID.fromString("a510d22f-f370-46c7-88e2-981573c427f5"));
    requisition = requisitionRepository.save(requisition);
  }

  @Test
  public void shouldUpdateOrderNumberConfiguration() {
    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration("prefix", true, true, true);

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

  @Test
  public void shouldUseNewestConfigurationWhenConvertingRequisitionToOrder() {
    final String prefix = "prefix";

    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration(prefix, false, false, false);

    postForOrderNumberConfiguration(orderNumberConfiguration, 200);

    orderNumberConfiguration.setIncludeOrderNumberPrefix(true);
    orderNumberConfiguration.setIncludeRequisitionTypeSuffix(true);
    postForOrderNumberConfiguration(orderNumberConfiguration, 200);

    mockReferenceData();

    ConvertToOrderDto convertToOrderDto =
        new ConvertToOrderDto(requisition.getId(), facility);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(Collections.singletonList(convertToOrderDto))
        .when()
        .post("/api/requisitions/convertToOrder")
        .then()
        .statusCode(201);

    Order order = orderRepository.findAll().iterator().next();

    String expected = prefix + requisition.getId().toString() + "E";
    assertEquals(expected, order.getOrderCode());

  }

  @Test
  public void shouldUseOldConfigurationWhenNewOneIsIncorrect() {

    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration("prefix", false, false, false);
    postForOrderNumberConfiguration(orderNumberConfiguration, 200);


    orderNumberConfiguration.setIncludeProgramCode(true);
    orderNumberConfiguration.setOrderNumberPrefix("p/refix.");
    orderNumberConfiguration.setIncludeRequisitionTypeSuffix(true);
    postForOrderNumberConfiguration(orderNumberConfiguration, 400);

    mockReferenceData();

    ConvertToOrderDto convertToOrderDto =
        new ConvertToOrderDto(requisition.getId(), facility);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(Collections.singletonList(convertToOrderDto))
        .when()
        .post("/api/requisitions/convertToOrder")
        .then()
        .statusCode(201);

    Order order = orderRepository.findAll().iterator().next();
    assertEquals(requisition.getId().toString(), order.getOrderCode());
  }

  @Test
  public void shouldReturn400WhenSavingConfigurationWithNotAlphanumericPrefix() {
    final String notAlphanumericString = "..dsa2,";

    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration(notAlphanumericString, true, false, false);

    postForOrderNumberConfiguration(orderNumberConfiguration, 400);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturn400WhenSavingConfigurationWithPrefixLongerThan8Characters() {
    final String tooLongPrefix = "123456789";

    OrderNumberConfiguration orderNumberConfiguration =
        new OrderNumberConfiguration(tooLongPrefix, true, false, false);

    postForOrderNumberConfiguration(orderNumberConfiguration, 400);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private void postForOrderNumberConfiguration(OrderNumberConfiguration orderNumberConfiguration,
                                               Integer code) {
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(orderNumberConfiguration)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(code);
  }

  private void mockReferenceData() {
    final String programFindResult = "{"
        + "\"id\":\"35316636-6264-6331-2d34-3933322d3462\","
        + "\"code\":\"" + programDto.getCode() + "\","
        + "\"name\":\"name\","
        + "\"description\":\"description\","
        + "\"active\":\"true\","
        + "\"periodsSkippable\":\"true\","
        + "\"showNonFullSupplyTab\":\"true\""
        + "}";

    wireMockRule.stubFor(get(urlMatching("/referencedata/api/programs/" + UUID_REGEX + ".*"))
        .willReturn(aResponse()
            .withHeader(CONTENT_TYPE, APPLICATION_JSON)
            .withBody(programFindResult)));

  }

}
