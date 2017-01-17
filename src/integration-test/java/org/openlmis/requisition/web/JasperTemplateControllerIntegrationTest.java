package org.openlmis.requisition.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.JasperTemplate;
import org.openlmis.requisition.dto.JasperTemplateDto;
import org.openlmis.requisition.repository.JasperTemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.MediaType;

import java.io.IOException;
import java.util.Arrays;
import java.util.UUID;

import guru.nidi.ramltester.junit.RamlMatchers;

@SuppressWarnings({"PMD.UnusedPrivateField"})
public class JasperTemplateControllerIntegrationTest extends BaseWebIntegrationTest {
  private static final String RESOURCE_URL = "/api/reports/templates/requisitions";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String TEMPLATE_CONTROLLER_TEST = "TemplateControllerIntegrationTest";
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");

  @Autowired
  private JasperTemplateRepository jasperTemplateRepository;

  private JasperTemplate jasperTemplate = new JasperTemplate();
  private JasperTemplateDto jasperTemplateDto;
  private Integer currentInstanceNumber;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;

    jasperTemplate.setId(UUID.randomUUID());
    jasperTemplate.setName(TEMPLATE_CONTROLLER_TEST + generateInstanceNumber());
    jasperTemplateDto = JasperTemplateDto.newInstance(jasperTemplate);
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }

  @Test
  public void shouldAddReportTemplate() throws IOException {
    ClassPathResource requisitionReport =
        new ClassPathResource("jasperTemplates/requisition.jrxml");

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.MULTIPART_FORM_DATA_VALUE)
        .multiPart("file", requisitionReport.getFilename(), requisitionReport.getInputStream())
        .formParam("name", TEMPLATE_CONTROLLER_TEST)
        .formParam("description", TEMPLATE_CONTROLLER_TEST)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(200);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldDeleteTemplate() {
    jasperTemplate = jasperTemplateRepository.save(jasperTemplate);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", jasperTemplate.getId())
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(204);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteNonexistentTemplate() {
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", jasperTemplate.getId())
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateTemplate() {
    jasperTemplateDto.setDescription(TEMPLATE_CONTROLLER_TEST);

    JasperTemplateDto response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", jasperTemplate.getId())
        .body(jasperTemplateDto)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(200)
        .extract().as(JasperTemplateDto.class);

    assertEquals(response.getDescription(), TEMPLATE_CONTROLLER_TEST);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewTemplateIfDoesNotExist() {
    jasperTemplateDto.setDescription(TEMPLATE_CONTROLLER_TEST);

    JasperTemplateDto response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", ID)
        .body(jasperTemplateDto)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(200)
        .extract().as(JasperTemplateDto.class);

    assertEquals(response.getDescription(), TEMPLATE_CONTROLLER_TEST);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllTemplates() {
    jasperTemplateRepository.save(jasperTemplate);

    JasperTemplateDto[] response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(RESOURCE_URL)
        .then()
        .statusCode(200)
        .extract().as(JasperTemplateDto[].class);

    Iterable<JasperTemplateDto> templates = Arrays.asList(response);
    assertTrue(templates.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenTemplate() {
    jasperTemplate = jasperTemplateRepository.save(jasperTemplate);

    JasperTemplateDto response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", jasperTemplate.getId())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(200)
        .extract().as(JasperTemplateDto.class);

    assertEquals(jasperTemplate.getId(), response.getId());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonexistentTemplate() {
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", jasperTemplate.getId())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
