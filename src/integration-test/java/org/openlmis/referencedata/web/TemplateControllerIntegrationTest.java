package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Test;
import org.openlmis.reporting.repository.TemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.MediaType;

import java.io.IOException;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

public class TemplateControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/templates";
  private static final String TEMPLATE_CONTROLLER_TEST = "TemplateControllerIntegrationTest";

  @Autowired
  private TemplateRepository templateRepository;

  @Test
  public void shouldAddReportTemplate() throws IOException {
    ClassPathResource podReport = new ClassPathResource("reports/podPrint.jrxml");

    restAssured.given()
        .queryParam("access_token", getToken())
        .contentType(MediaType.MULTIPART_FORM_DATA_VALUE)
        .multiPart("file", podReport.getFilename(), podReport.getInputStream())
        .formParam("name", TEMPLATE_CONTROLLER_TEST)
        .formParam("description", TEMPLATE_CONTROLLER_TEST)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(200);

    assertNotNull(templateRepository.findByName(TEMPLATE_CONTROLLER_TEST));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
