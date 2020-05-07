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

import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import guru.nidi.ramltester.junit.RamlMatchers;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.hibernate.exception.ConstraintViolationException;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.dto.RequisitionTemplateDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.i18n.MessageKeys;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.validation.Errors;

@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionTemplateControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/requisitionTemplates";
  private static final String ID_URL = RESOURCE_URL + "/{id}";

  private RequisitionTemplate template;
  private RequisitionTemplateDto templateDto;

  @Before
  public void setUp() {
    template = new RequisitionTemplateDataBuilder()
        .withAssignment(UUID.randomUUID(), UUID.randomUUID())
        .withRequiredColumns()
        .build();
    templateDto = dtoBuilder.newInstance(template);

    mockUserAuthenticated();

    given(requisitionTemplateRepository.findById(template.getId()))
        .willReturn(Optional.of(template));

    // Mock saving objects
    given(requisitionTemplateRepository.save(any(RequisitionTemplate.class)))
        .willAnswer(new SaveAnswer<>());
  }

  // GET /api/requisitionTemplates

  @Test
  public void shouldGetActiveRequisitionTemplates() {
    // given
    RequisitionTemplate another = new RequisitionTemplateDataBuilder()
        .withAssignment(UUID.randomUUID(), null)
        .build();
    List<RequisitionTemplate> templates = Arrays.asList(template, another);
    given(requisitionTemplateRepository.getActiveTemplates()).willReturn(templates);
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    // when
    RequisitionTemplateDto[] result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .when()
        .get(RESOURCE_URL)
        .then()
        .statusCode(200)
        .extract()
        .as(RequisitionTemplateDto[].class);

    // then
    assertNotNull(result);
    assertEquals(2, result.length);
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test(expected = IllegalArgumentException.class)
  public void shouldNotAllowPaginationWithZeroSize() {
    // given
    RequisitionTemplate another = new RequisitionTemplateDataBuilder()
            .withAssignment(UUID.randomUUID(), null)
            .build();
    Pageable page = PageRequest.of(0, 0);
    List<RequisitionTemplate> templates = Arrays.asList(template, another);
    given(requisitionTemplateRepository.getActiveTemplates()).willReturn(templates);

    // when
    restAssured.given()
            .queryParam("page", page.getPageNumber())
            .queryParam("size", page.getPageSize())
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .when()
            .get(RESOURCE_URL)
            .then()
            .statusCode(400);
  }

  @Test(expected = IllegalArgumentException.class)
  public void shouldNotAllowPaginationWithoutSize() {
    // given
    RequisitionTemplate another = new RequisitionTemplateDataBuilder()
            .withAssignment(UUID.randomUUID(), null)
            .build();
    Pageable page = PageRequest.of(0, 0);
    List<RequisitionTemplate> templates = Arrays.asList(template, another);
    given(requisitionTemplateRepository.getActiveTemplates()).willReturn(templates);

    // when
    restAssured.given()
            .queryParam("page", page.getPageNumber())
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .when()
            .get(RESOURCE_URL)
            .then()
            .statusCode(400);
  }

  // POST /api/requisitionTemplates

  @Test
  public void shouldCreateRequisitionTemplate() {
    // given
    mockValidationSuccess();
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(templateDto)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(201);

    // then
    verify(requisitionTemplateRepository, atLeastOnce()).save(any(RequisitionTemplate.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotCreateRequisitionTemplateIfNameIsDuplicated() {
    // given
    when(requisitionTemplateRepository.save(any(RequisitionTemplate.class)))
        .thenThrow(new DataIntegrityViolationException("test",
            new ConstraintViolationException("", null, "requisition_template_name_unique_idx")));
    mockValidationSuccess();
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(templateDto)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(400)
        .body("messageKey", is(MessageKeys.ERROR_TEMPLATE_NAME_DUPLICATION));

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // GET /api/requisitionTemplates/{id}

  @Test
  public void shouldGetChosenRequisitionTemplate() {
    // given
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    // when
    RequisitionTemplateDto result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", template.getId())
        .when()
        .get(ID_URL)
        .then()
        .statusCode(200)
        .extract()
        .as(RequisitionTemplateDto.class);

    // then
    assertEquals(template.getId(), result.getId());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotGetNonExistentRequisitionTemplate() {
    // given
    given(requisitionTemplateRepository.findById(anyUuid())).willReturn(Optional.empty());
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
    mockValidationSuccess();
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();
    given(requisitionRepository.findByTemplateId(template.getId()))
        .willReturn(Collections.emptyList());

    RequisitionTemplate newTemplate = new RequisitionTemplateDataBuilder()
        .withName("new_test_name")
        .withNumberOfPeriodsToAverage(100)
        .withAssignment(template.getProgramId(), null)
        .build();

    RequisitionTemplateDto newTemplateDto = dtoBuilder.newInstance(newTemplate);

    // when
    RequisitionTemplateDto result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", template.getId())
        .body(newTemplateDto)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(200)
        .extract()
        .as(RequisitionTemplateDto.class);

    // then
    assertEquals(template.getId(), result.getId());
    assertEquals(newTemplate.getNumberOfPeriodsToAverage(), result.getNumberOfPeriodsToAverage());
    assertEquals(newTemplateDto.getName(), result.getName());
    verify(requisitionTemplateRepository, never()).saveAndFlush(any(RequisitionTemplate.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldSaveNewRequisitionTemplateIfItHasSomeRequisitions() {
    // given
    mockValidationSuccess();
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();
    given(requisitionRepository.findByTemplateId(template.getId()))
        .willReturn(Collections.singletonList(mock(Requisition.class)));

    RequisitionTemplate newTemplate = new RequisitionTemplateDataBuilder()
        .withAssignment(template.getProgramId(), UUID.randomUUID())
        .build();

    RequisitionTemplateDto newTemplateDto = dtoBuilder.newInstance(newTemplate);

    // when
    RequisitionTemplateDto result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", template.getId())
        .body(newTemplateDto)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(200)
        .extract()
        .as(RequisitionTemplateDto.class);

    // then
    assertThat(result.getId(), is(not(template.getId())));

    ArgumentCaptor<RequisitionTemplate> templateCaptor = ArgumentCaptor
        .forClass(RequisitionTemplate.class);

    verify(requisitionTemplateRepository).saveAndFlush(templateCaptor.capture());
    assertThat(templateCaptor.getValue(), hasProperty("archived", is(true)));

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldCreateNewRequisitionTemplateIfDoesNotExist() {
    // given
    given(requisitionTemplateRepository.findById(anyUuid())).willReturn(Optional.empty());
    mockValidationSuccess();
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    given(requisitionTemplateRepository.findById(templateDto.getId())).willReturn(null);

    // when
    RequisitionTemplateDto result = restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", UUID.randomUUID())
        .body(templateDto)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(200)
        .extract()
        .as(RequisitionTemplateDto.class);

    // then
    assertEquals(templateDto.getProgramId(), result.getProgramId());
    assertEquals(templateDto.getFacilityTypeIds(), result.getFacilityTypeIds());
    verify(requisitionTemplateRepository).save(any(RequisitionTemplate.class));
    verify(requisitionTemplateRepository, never()).saveAndFlush(any(RequisitionTemplate.class));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldNotUpdateRequisitionTemplateIfNameIsDuplicated() {
    // given
    when(requisitionTemplateRepository.save(any(RequisitionTemplate.class)))
        .thenThrow(new DataIntegrityViolationException("test",
            new ConstraintViolationException("", null, "requisition_template_name_unique_idx")));
    mockValidationSuccess();
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", template.getId())
        .body(templateDto)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(400)
        .body("messageKey", is(MessageKeys.ERROR_TEMPLATE_NAME_DUPLICATION));

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  // DELETE /api/requisitionTemplates/{id}

  @Test
  public void shouldDeleteRequisitionTemplate() {
    // given
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
    given(requisitionTemplateRepository.findById(anyUuid())).willReturn(Optional.empty());
    doReturn(ValidationResult.success()).when(permissionService).canManageRequisitionTemplate();

    // when
    restAssured.given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
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

  // Helper methods

  private void mockValidationSuccess() {
    given(requisitionTemplateDtoValidator.supports(any(Class.class))).willReturn(true);
    doNothing().when(requisitionTemplateDtoValidator).validate(anyObject(), any(Errors.class));
  }

}
