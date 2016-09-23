package org.openlmis.requisition.web;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.reporting.repository.TemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;

import guru.nidi.ramltester.junit.RamlMatchers;

import java.io.IOException;

public class ProofOfDeliveryTemplateControllerComponentTest extends BaseWebComponentTest {

  private static final String RESOURCE_URL = "/api/proofOfDeliveryTemplates";
  private static final String ACCESS_TOKEN = "access_token";
  private static final String POD_TEMPLATE = "Print POD";
  private ClassPathResource podReport;

  @Autowired
  private TemplateRepository templateRepository;

  @Before
  public void setUp() throws IOException {
    podReport = new ClassPathResource("reports/podPrint.jrxml");
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType("multipart/form-data")
        .multiPart("file", podReport.getFilename(), podReport.getInputStream())
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(200);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldSavePodReportTemplate() {
    assertNotNull(templateRepository.findByName(POD_TEMPLATE));
  }

  @Test
  public void shouldDownloadPodTeportTemplate() {
    restAssured = ramlDefinition.createRestAssured();
    String jrxml = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType("application/xml")
        .when()
        .get(RESOURCE_URL)
        .then()
        .statusCode(200)
        .extract()
        .body()
        .asString();

    Assert.assertNotNull(jrxml);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
