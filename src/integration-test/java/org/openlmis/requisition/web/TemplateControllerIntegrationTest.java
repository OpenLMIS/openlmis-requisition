package org.openlmis.requisition.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.BDDMockito.given;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.openlmis.requisition.domain.Template;
import org.openlmis.requisition.dto.TemplateDto;
import org.openlmis.requisition.repository.TemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.MediaType;

import guru.nidi.ramltester.junit.RamlMatchers;

import java.io.IOException;
import java.util.Arrays;
import java.util.UUID;

@SuppressWarnings({"PMD.UnusedPrivateField"})
@Ignore
public class TemplateControllerIntegrationTest extends BaseWebIntegrationTest {
  private static final String RESOURCE_URL = "/api/templates";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String ACCESS_TOKEN = "access_token";
  private static final String TEMPLATE_CONTROLLER_TEST = "TemplateControllerIntegrationTest";
  private static final UUID ID = UUID.fromString("1752b457-0a4b-4de0-bf94-5a6a8002427e");

  @Autowired
  private TemplateRepository templateRepository;

  private Template template = new Template();
  private TemplateDto templateDto;
  private Integer currentInstanceNumber;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;

    template.setId(UUID.randomUUID());
    template.setName(TEMPLATE_CONTROLLER_TEST + generateInstanceNumber());
    templateDto = TemplateDto.newInstance(template);
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }

  @Test
  public void shouldAddReportTemplate() throws IOException {
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

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldDeleteTemplate() {
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", template.getId())
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(204);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteNonexistentTemplate() {
    given(templateRepository.findOne(template.getId())).willReturn(null);
    given(templateRepository.exists(template.getId())).willReturn(false);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", template.getId())
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldUpdateTemplate() {

    templateDto.setDescription(TEMPLATE_CONTROLLER_TEST);

    TemplateDto response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", template.getId())
        .body(templateDto)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(200)
        .extract().as(TemplateDto.class);

    assertEquals(response.getDescription(), TEMPLATE_CONTROLLER_TEST);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewTemplateIfDoesNotExist() {
    given(templateRepository.findOne(template.getId())).willReturn(template);
    given(templateRepository.exists(template.getId())).willReturn(true);

    templateDto.setDescription(TEMPLATE_CONTROLLER_TEST);

    TemplateDto response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", ID)
        .body(templateDto)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(200)
        .extract().as(TemplateDto.class);

    assertEquals(response.getDescription(), TEMPLATE_CONTROLLER_TEST);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetAllTemplates() {
    TemplateDto[] response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(RESOURCE_URL)
        .then()
        .statusCode(200)
        .extract().as(TemplateDto[].class);

    Iterable<TemplateDto> templates = Arrays.asList(response);
    assertTrue(templates.iterator().hasNext());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetChosenTemplate() {

    TemplateDto response = restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", template.getId())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(200)
        .extract().as(TemplateDto.class);

    assertEquals(template.getId(), response.getId());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonexistentTemplate() {
    given(templateRepository.findOne(template.getId())).willReturn(null);
    given(templateRepository.exists(template.getId())).willReturn(false);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", template.getId())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
