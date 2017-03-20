/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

package org.openlmis.requisition.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Ignore;
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

@SuppressWarnings({"PMD.UnusedPrivateField","PMD.TooManyMethods"})
public class JasperTemplateControllerIntegrationTest extends BaseWebIntegrationTest {
  private static final String RESOURCE_URL = "/api/reports/templates/requisitions";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String FORMAT_PARAM = "format";
  private static final String REPORT_URL = ID_URL + "/{" + FORMAT_PARAM + "}";
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
  
  @Test
  @Ignore //Once the integration tests actually mock services, this should be re-enabled with mocks
  public void generateReportShouldReturnReports() {

    jasperTemplate = jasperTemplateRepository.save(jasperTemplate);

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", jasperTemplate.getId())
        .pathParam(FORMAT_PARAM, "pdf")
        .when()
        .get(REPORT_URL)
        .then()
        .statusCode(200);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", jasperTemplate.getId())
        .pathParam(FORMAT_PARAM, "csv")
        .when()
        .get(REPORT_URL)
        .then()
        .statusCode(200);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", jasperTemplate.getId())
        .pathParam(FORMAT_PARAM, "xls")
        .when()
        .get(REPORT_URL)
        .then()
        .statusCode(200);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .pathParam("id", jasperTemplate.getId())
        .pathParam(FORMAT_PARAM, "html")
        .when()
        .get(REPORT_URL)
        .then()
        .statusCode(200);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void generateReportShouldReturnNotFoundIfReportTemplateDoesNotExist() {

    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", jasperTemplate.getId())
        .pathParam(FORMAT_PARAM, "pdf")
        .when()
        .get(REPORT_URL)
        .then()
        .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
