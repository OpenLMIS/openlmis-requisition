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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.validate.RequisitionTemplateValidator;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.validation.Errors;

import guru.nidi.ramltester.junit.RamlMatchers;

import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionTemplateControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/requisitionTemplates";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String ID_URL = RESOURCE_URL + "/{id}";

  @MockBean
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @MockBean
  private RequisitionTemplateValidator requisitionTemplateValidator;

  @Before
  public void setUp() {
    mockUserAuthenticated();

    // Mock saving objects
    given(requisitionTemplateRepository.save(any(RequisitionTemplate.class)))
        .willAnswer(new SaveAnswer<>());
  }

  // GET /api/requisitionTemplates

  @Test
  public void shouldGetAllRequisitionTemplates() {
    // given
    List<RequisitionTemplate> templates = Arrays.asList(generateTemplate(), generateTemplate());
    given(requisitionTemplateRepository.findAll()).willReturn(templates);
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    // when
    RequisitionTemplate[] result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(APPLICATION_JSON)
        .when()
        .get(RESOURCE_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionTemplate[].class);

    // then
    assertNotNull(result);
    assertEquals(2, result.length);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // POST /api/requisitionTemplates

  @Test
  public void shouldCreateRequisitionTemplate() {
    // given
    RequisitionTemplate template = generateTemplate(false);
    mockValidationSuccess();
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(APPLICATION_JSON)
        .body(template)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(201);

    // then
    verify(requisitionTemplateRepository, atLeastOnce()).save(any(RequisitionTemplate.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/requisitionTemplates/{id}

  @Test
  public void shouldGetChosenRequisitionTemplate() {
    // given
    RequisitionTemplate template = generateTemplate();
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    // when
    RequisitionTemplate result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(APPLICATION_JSON)
        .pathParam("id", template.getId())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionTemplate.class);

    // then
    assertEquals(template.getId(), result.getId());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonExistentRequisitionTemplate() {
    // given
    given(requisitionTemplateRepository.findOne(anyUuid())).willReturn(null);
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", UUID.randomUUID())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(404);

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // PUT /api/requisitionTemplates/{id}

  @Test
  public void shouldUpdateRequisitionTemplate() {
    // given
    RequisitionTemplate oldTemplate = generateTemplate();
    oldTemplate.setNumberOfPeriodsToAverage(10);

    RequisitionTemplate newTemplate = generateTemplate(false);
    newTemplate.setNumberOfPeriodsToAverage(100);
    mockValidationSuccess();
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    // when
    RequisitionTemplate result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(APPLICATION_JSON)
        .pathParam("id", oldTemplate.getId())
        .body(newTemplate)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionTemplate.class);

    // then
    assertEquals(oldTemplate.getId(), result.getId());
    assertEquals(newTemplate.getNumberOfPeriodsToAverage(), result.getNumberOfPeriodsToAverage());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewRequisitionTemplateIfDoesNotExist() {
    // given
    given(requisitionTemplateRepository.findOne(anyUuid())).willReturn(null);
    mockValidationSuccess();
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    RequisitionTemplate template = generateTemplate(false);

    // when
    RequisitionTemplate result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(APPLICATION_JSON)
        .pathParam("id", UUID.randomUUID())
        .body(template)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionTemplate.class);

    // then
    assertEquals(template.getProgramId(), result.getProgramId());
    verify(requisitionTemplateRepository, atLeastOnce()).save(any(RequisitionTemplate.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldDeleteRequisitionTemplate() {
    // given
    RequisitionTemplate template = generateTemplate();
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", template.getId())
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(204);

    // then
    verify(requisitionTemplateRepository, atLeastOnce()).delete(template);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteNonExistentRequisitionTemplate() {
    // given
    given(requisitionTemplateRepository.findOne(anyUuid())).willReturn(null);
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(APPLICATION_JSON)
        .pathParam("id", UUID.randomUUID())
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(404);

    // then
    verify(requisitionTemplateRepository, never()).delete(any(RequisitionTemplate.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotDeleteRequisitionTemplateWhenRelatedRequisitionsExist() {
    // given
    RequisitionTemplate template = generateTemplate();
    List<Requisition> requisitions = Collections.singletonList(generateRequisition());
    given(requisitionRepository.findByTemplateId(template.getId())).willReturn(requisitions);
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", template.getId())
        .when()
        .delete(ID_URL)
        .then()
        .statusCode(400);

    // then
    verify(requisitionTemplateRepository, never()).delete(template);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/requisitionTemplates/search

  @Test
  public void shouldFindRequisitionTemplateByProgram() {
    // given
    RequisitionTemplate template = generateTemplate();
    given(requisitionTemplateRepository.getTemplateForProgram(template.getProgramId()))
        .willReturn(template);
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    // when
    RequisitionTemplate result = restAssured.given()
        .queryParam("program", template.getProgramId())
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(RequisitionTemplate.class);

    // then
    assertEquals(template.getId(), result.getId());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturn403WhenUserHasNoRightsToSearchForRequisitionTemplates() {
    // given
    doReturn(ValidationResult.noPermission("noAccess")).when(permissionService)
        .canManageRequisitionTemplate();

    // when
    restAssured.given()
        .queryParam("program", UUID.randomUUID())
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(403);

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // Helper methods

  private void mockValidationSuccess() {
    given(requisitionTemplateValidator.supports(any(Class.class))).willReturn(true);
    doNothing().when(requisitionTemplateValidator).validate(anyObject(), any(Errors.class));
  }

  private RequisitionTemplate generateTemplate() {
    return generateTemplate(true);
  }

  private RequisitionTemplate generateTemplate(boolean persistent) {
    RequisitionTemplate template = new RequisitionTemplate();

    template.setId(UUID.randomUUID());
    template.setProgramId(UUID.randomUUID());
    template.setCreatedDate(ZonedDateTime.now());
    template.setColumnsMap(new HashMap<>());

    if (persistent) {
      given(requisitionTemplateRepository.findOne(template.getId())).willReturn(template);
    }

    return template;
  }
}
