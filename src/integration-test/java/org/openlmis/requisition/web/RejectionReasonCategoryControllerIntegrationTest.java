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

import static java.util.Collections.singletonList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

import guru.nidi.ramltester.junit.RamlMatchers;
import java.util.Collections;
import java.util.Optional;
import java.util.UUID;

import org.hibernate.exception.ConstraintViolationException;
import org.junit.Test;
import org.openlmis.requisition.domain.RejectionReasonCategory;
import org.openlmis.requisition.dto.RejectionReasonCategoryDto;
import org.openlmis.requisition.service.PageDto;
import org.openlmis.requisition.testutils.RejectionReasonCategoryDataBuilder;
import org.openlmis.requisition.utils.Pagination;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;



@SuppressWarnings({"PMD.TooManyMethods"})
public class RejectionReasonCategoryControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/rejectionReasonCategories";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String REJECTION_REASON_CATEGORY_NAME_ONE = "category1";
  private static final String REJECTION_REASON_CATEGORY_CODE_ONE = "code1";

  private RejectionReasonCategory rejectionReasonCategory1;
  private RejectionReasonCategoryDto rejectionReasonCategoryDto;
  private UUID rejectionReasonCategoryId;
  private Pageable pageable;

  /**
   * Constructor for test class.
   */
  public RejectionReasonCategoryControllerIntegrationTest() {
    rejectionReasonCategory1 = new RejectionReasonCategoryDataBuilder()
            .withCode(REJECTION_REASON_CATEGORY_CODE_ONE)
            .withName(REJECTION_REASON_CATEGORY_NAME_ONE)
            .withActive(true)
            .buildAsNew();

    rejectionReasonCategoryDto = new RejectionReasonCategoryDto();
    rejectionReasonCategory1.export(rejectionReasonCategoryDto);
    rejectionReasonCategoryId = UUID.randomUUID();
    pageable = PageRequest.of(0, 10);
  }

  @Test
  public void shouldReturnPageOfRejectionReasonCategory() {
    doReturn(Pagination.getPage(singletonList(rejectionReasonCategory1), pageable))
            .when(rejectionReasonCategoryRepository).findAll();

    PageDto resultPage = restAssured.given()
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .queryParam("page", pageable.getPageNumber())
            .queryParam("size", pageable.getPageSize())
            .when()
            .get(RESOURCE_URL)
            .then()
            .statusCode(200)
            .extract().as(PageDto.class);

    assertEquals(1, resultPage.getContent().size());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldGetRejectionReasonCategory() {

    given(rejectionReasonCategoryRepository.findById(rejectionReasonCategoryId))
            .willReturn(Optional.of(rejectionReasonCategory1));

    RejectionReasonCategoryDto response = restAssured
            .given()
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .pathParam("id", rejectionReasonCategoryId)
            .when()
            .get(ID_URL)
            .then()
            .statusCode(200)
            .extract().as(RejectionReasonCategoryDto.class);

    assertEquals(REJECTION_REASON_CATEGORY_NAME_ONE, response.getName());
    assertEquals(REJECTION_REASON_CATEGORY_CODE_ONE, response.getCode());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void getShouldReturnNotFoundForNonExistingRejectionReasonCategory() {
    given(rejectionReasonRepository.findById(rejectionReasonCategoryId))
            .willReturn(Optional.empty());

    restAssured
            .given()
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .pathParam("id", rejectionReasonCategoryId)
            .when()
            .get(ID_URL)
            .then()
            .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldPostRejectionReasonCategory() {
    given(rejectionReasonCategoryRepository.save(any()))
            .willReturn(rejectionReasonCategory1);

    RejectionReasonCategoryDto response = restAssured
            .given()
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .body(rejectionReasonCategoryDto)
            .when()
            .post(RESOURCE_URL)
            .then()
            .statusCode(201)
            .extract().as(RejectionReasonCategoryDto.class);

    assertEquals(REJECTION_REASON_CATEGORY_NAME_ONE, response.getName());
    assertEquals(REJECTION_REASON_CATEGORY_CODE_ONE, response.getCode());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturnBadRequestWhenPostEmptyFields() {
    // given
    when(rejectionReasonCategoryRepository.save(any(RejectionReasonCategory.class)))
            .thenThrow(new DataIntegrityViolationException("test",
                    new ConstraintViolationException("", null, "missing mandatory field")));

    restAssured
            .given()
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .body(rejectionReasonCategoryDto)
            .when()
            .post(RESOURCE_URL)
            .then()
            .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());

  }

  @Test
  public void shouldPutRejectionReasonCategory() {

    when(rejectionReasonCategoryRepository.findById(rejectionReasonCategoryId))
            .thenReturn(Optional.of(rejectionReasonCategory1));
    given(rejectionReasonCategoryRepository.save(any()))
            .willReturn(rejectionReasonCategory1);

    RejectionReasonCategoryDto response = restAssured
            .given()
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", rejectionReasonCategoryId)
            .body(rejectionReasonCategoryDto)
            .when()
            .put(ID_URL)
            .then()
            .statusCode(200)
            .extract().as(RejectionReasonCategoryDto.class);

    assertEquals(REJECTION_REASON_CATEGORY_NAME_ONE, response.getName());
    assertEquals(REJECTION_REASON_CATEGORY_CODE_ONE, response.getCode());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturnBadRequestWhenPutEmptyFields() {
    // given
    when(rejectionReasonCategoryRepository.save(any(RejectionReasonCategory.class)))
            .thenThrow(new DataIntegrityViolationException("test",
                    new ConstraintViolationException("", null, "missing mandatory field")));

    restAssured
            .given()
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", rejectionReasonCategoryId)
            .body(rejectionReasonCategoryDto)
            .when()
            .put(ID_URL)
            .then()
            .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());

  }

  @Test
  public void shouldSearchRejectionReasonCategory() {

    given(rejectionReasonCategoryRepository.searchRejectionReasonCategory(
            REJECTION_REASON_CATEGORY_NAME_ONE,
            REJECTION_REASON_CATEGORY_CODE_ONE))
            .willReturn(Collections.singleton(rejectionReasonCategory1));

    RejectionReasonCategoryDto[] response = restAssured
            .given()
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .queryParam("name", REJECTION_REASON_CATEGORY_NAME_ONE)
            .queryParam("code", REJECTION_REASON_CATEGORY_CODE_ONE)
            .when()
            .get(SEARCH_URL)
            .then()
            .statusCode(200)
            .extract().as(RejectionReasonCategoryDto[].class);

    RejectionReasonCategoryDto rejectionReasonCategory = response[0];
    assertEquals(REJECTION_REASON_CATEGORY_NAME_ONE, rejectionReasonCategory.getName());
    assertEquals(REJECTION_REASON_CATEGORY_CODE_ONE, rejectionReasonCategory.getCode());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturnBadRequestWhenSearchWithAllParameterNull() {
    // when
    restAssured.given()
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .when()
            .get(SEARCH_URL)
            .then()
            .statusCode(400);

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

}
