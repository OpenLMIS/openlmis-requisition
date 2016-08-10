package org.openlmis.referencedata.web;


import com.jayway.restassured.RestAssured;
import guru.nidi.ramltester.RamlDefinition;
import guru.nidi.ramltester.RamlLoaders;
import guru.nidi.ramltester.junit.RamlMatchers;
import guru.nidi.ramltester.restassured.RestAssuredClient;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.reporting.repository.TemplateParameterRepository;
import org.openlmis.reporting.repository.TemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.web.util.UriComponentsBuilder;

import java.io.IOException;

public class TemplateControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = System.getenv("BASE_URL")
      + "/api/templates";

  private static final String TEMPLATE_CONTROLLER_TEST = "TemplateControllerIntegrationTest";

  @Autowired
  private TemplateRepository templateRepository;

  @Autowired
  private TemplateParameterRepository templateParameterRepository;

  private static final String RAML_ASSERT_MESSAGE = "HTTP request/response should match RAML "
      + "definition.";
  private RamlDefinition ramlDefinition;
  private RestAssuredClient restAssured;

  /**
   * Prepare the test environment.
   */
  @Before
  public void setUp() {
    RestAssured.baseURI = BASE_URL;
    ramlDefinition = RamlLoaders.fromClasspath().load("api-definition-raml.yaml");
    restAssured = ramlDefinition.createRestAssured();
  }

  /**
   * Cleanup the test environment.
   */
  @After
  public void cleanUp() {
    templateParameterRepository.deleteAll();
    templateRepository.deleteAll();
  }

  @Test
  public void testAddReportTemplate() throws IOException {
    ClassPathResource podReport = new ClassPathResource("reports/podPrint.jrxml");
    UriComponentsBuilder builder = UriComponentsBuilder.fromHttpUrl(RESOURCE_URL)
        .queryParam("access_token", getToken());

    restAssured.given().contentType("multipart/form-data")
        .multiPart("file", podReport.getFilename(), podReport.getInputStream())
        .formParam("name", TEMPLATE_CONTROLLER_TEST)
        .formParam("description", TEMPLATE_CONTROLLER_TEST)
        .when().post(builder.toUriString()).then().statusCode(200);

    Assert.assertNotNull(templateRepository.findByName(TEMPLATE_CONTROLLER_TEST));
    Assert.assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(),
        RamlMatchers.hasNoViolations());
  }
}
