package org.openlmis.referencedata.web;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.reporting.model.Template;
import org.openlmis.reporting.repository.TemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.MediaType;

import java.io.IOException;
import java.util.Arrays;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

public class TemplateControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/templates";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String ACCESS_TOKEN = "access_token";
  private static final String TEMPLATE_CONTROLLER_TEST = "TemplateControllerIntegrationTest";

  @Autowired
  private TemplateRepository templateRepository;

  private Template template = new Template();
  private Integer currentInstanceNumber;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    template.setName(TEMPLATE_CONTROLLER_TEST + generateInstanceNumber());
    templateRepository.save(template);
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }

  @Test
  public void testShouldAddReportTemplate() throws IOException {
    ClassPathResource podReport = new ClassPathResource("reports/podPrint.jrxml");

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
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

  @Test
  public void testShouldDeleteTemplate() {

    restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", template.getId())
          .when()
          .delete(ID_URL)
          .then()
          .statusCode(204);

    assertFalse(templateRepository.exists(template.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldUpdateRequisitionTemplate() {

    template.setDescription(TEMPLATE_CONTROLLER_TEST);

    Template response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", template.getId())
          .body(template)
          .when()
          .put(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(Template.class);

    assertEquals(response.getDescription(), TEMPLATE_CONTROLLER_TEST);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldGetAllRequisitionTemplates() {

    Template[] response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .when()
          .get(RESOURCE_URL)
          .then()
          .statusCode(200)
          .extract().as(Template[].class);

    Iterable<Template> templates = Arrays.asList(response);
    assertTrue(templates.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void testShouldGetChosenRequisitionTemplate() {

    Template response = restAssured.given()
          .queryParam(ACCESS_TOKEN, getToken())
          .contentType(MediaType.APPLICATION_JSON_VALUE)
          .pathParam("id", template.getId())
          .when()
          .get(ID_URL)
          .then()
          .statusCode(200)
          .extract().as(Template.class);

    assertTrue(templateRepository.exists(response.getId()));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
