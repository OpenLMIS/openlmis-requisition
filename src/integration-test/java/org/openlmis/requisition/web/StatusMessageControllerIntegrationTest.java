package org.openlmis.requisition.web;


import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import guru.nidi.ramltester.junit.RamlMatchers;

import java.time.ZonedDateTime;
import java.util.UUID;

public class StatusMessageControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/requisitions";
  private static final String STATUS_MESSAGE_URL = RESOURCE_URL + "/{id}/statusMessages";

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  private Requisition requisition;

  @Before
  public void setUp() {
    requisition = generateRequisition();
  }


  @Test
  public void shouldReturn403IfUserHasNoRightsToViewRequisitionFromWhichStatusMessageIs() {
    denyUserAllRights();

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", requisition.getId())
        .when()
        .get(STATUS_MESSAGE_URL)
        .then()
        .statusCode(403);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  private void denyUserAllRights() {
    wireMockRule.stubFor(
        get(urlMatching(REFERENCEDATA_API_USERS + UUID_REGEX + "/hasRight.*"))
            .willReturn(aResponse()
                .withHeader(CONTENT_TYPE, APPLICATION_JSON)
                .withBody("{ \"result\":\"false\" }"))
    );
  }

  private Requisition generateRequisition() {
    RequisitionTemplate template = requisitionTemplateRepository.save(new RequisitionTemplate());
    Requisition requisition = new Requisition(UUID.randomUUID(), UUID.randomUUID(),
        UUID.randomUUID(), UUID.randomUUID(), RequisitionStatus.INITIATED, true);

    requisition.setId(UUID.randomUUID());
    requisition.setCreatorId(UUID.randomUUID());
    requisition.setCreatedDate(ZonedDateTime.now());
    requisition.setTemplate(template);
    requisition.setNumberOfMonthsInPeriod(1);
    return requisitionRepository.save(requisition);
  }
}
