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
import org.openlmis.requisition.domain.RejectionReason;
import org.openlmis.requisition.domain.RejectionReasonCategory;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.RejectionReasonDto;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.openlmis.requisition.service.PageDto;
import org.openlmis.requisition.testutils.RejectionReasonCategoryDataBuilder;
import org.openlmis.requisition.testutils.RejectionReasonDataBuilder;
import org.openlmis.requisition.utils.Pagination;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;


@SuppressWarnings({"PMD.TooManyMethods"})
public class RejectionReasonControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/rejectionReasons";
  private static final String ID_URL = RESOURCE_URL + "/{id}";
  private static final String SEARCH_URL = RESOURCE_URL + "/search";
  private static final String REJECTION_REASON_NAME_ONE = "reason1";
  private static final String REJECTION_REASON_CODE_ONE = "RRC6";
  private static final String REJECTION_REASON_CATEGORY_NAME = "cat1";
  private static final String REJECTION_REASON_CATEGORY_CODE = "code1";

  private RejectionReason rejectionReason;
  private RejectionReasonCategory rejectionReasonCategory;
  private RejectionReasonDto rejectionReasonDto;
  private UUID rejectionReasonId;
  private Pageable pageable;

  /**
   * Constructor for test class.
   */
  public RejectionReasonControllerIntegrationTest() {

    rejectionReasonCategory = new RejectionReasonCategoryDataBuilder()
            .withCode(REJECTION_REASON_CATEGORY_CODE)
            .withName(REJECTION_REASON_CATEGORY_NAME)
            .withActive(true)
            .buildAsNew();
    rejectionReason = new RejectionReasonDataBuilder()
            .withName(REJECTION_REASON_NAME_ONE)
            .withCategory(rejectionReasonCategory)
            .withCode(REJECTION_REASON_CODE_ONE)
            .withActive(true)
            .buildAsNew();

    rejectionReasonDto = new RejectionReasonDto();
    rejectionReason.export(rejectionReasonDto);
    rejectionReasonId = UUID.randomUUID();
    pageable = PageRequest.of(0, 10);
  }

  @Test
  public void shouldReturnPageOfRejectionReason() {
    doReturn(Pagination.getPage(singletonList(rejectionReason), pageable))
            .when(rejectionReasonRepository).findAll();

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
  public void shouldGetRejectionReason() {

    given(rejectionReasonRepository.findById(rejectionReasonId))
            .willReturn(Optional.of(rejectionReason));

    RejectionReasonDto response = restAssured
        .given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .pathParam("id", rejectionReasonId)
        .when()
        .get(ID_URL)
        .then()
        .statusCode(200)
        .extract().as(RejectionReasonDto.class);

    assertEquals(REJECTION_REASON_NAME_ONE, response.getName());
    assertEquals(REJECTION_REASON_CODE_ONE, response.getCode());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldPostRejectionReason() {

    given(rejectionReasonRepository.findById(rejectionReasonId))
            .willReturn(null);
    given(rejectionReasonRepository.save(rejectionReason)).willReturn(rejectionReason);

    RejectionReasonDto response = restAssured
        .given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .body(rejectionReasonDto)
        .when()
        .post(RESOURCE_URL)
        .then()
        .statusCode(201)
        .extract().as(RejectionReasonDto.class);

    assertEquals(REJECTION_REASON_NAME_ONE, response.getName());
    assertEquals(REJECTION_REASON_CODE_ONE, response.getCode());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturnBadRequestWhenPostEmptyFields(){
    // given
    when(rejectionReasonRepository.save(any(RejectionReason.class)))
            .thenThrow(new DataIntegrityViolationException("test",
                    new ConstraintViolationException("", null, "missing mandatory field")));
    restAssured
            .given()
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .body(rejectionReasonDto)
            .when()
            .post(RESOURCE_URL)
            .then()
            .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());

  }

  @Test
  public void shouldPutRejectionReason() {

    when(rejectionReasonRepository.findById(rejectionReasonId))
            .thenReturn(Optional.of(rejectionReason));
    given(rejectionReasonRepository.save(rejectionReason)).willReturn(rejectionReason);

    RejectionReasonDto response = restAssured
        .given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .pathParam("id", rejectionReasonId)
        .body(rejectionReasonDto)
        .when()
        .put(ID_URL)
        .then()
        .statusCode(200)
        .extract().as(RejectionReasonDto.class);

    assertEquals(REJECTION_REASON_NAME_ONE, response.getName());
    assertEquals(REJECTION_REASON_CODE_ONE, response.getCode());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldReturnBadRequestWhenPutEmptyFields(){
    // given
    when(rejectionReasonRepository.save(any(RejectionReason.class)))
            .thenThrow(new DataIntegrityViolationException("test",
                    new ConstraintViolationException("", null, "missing mandatory field")));

    restAssured
            .given()
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .pathParam("id", rejectionReasonId)
            .body(rejectionReasonDto)
            .when()
            .put(ID_URL)
            .then()
            .statusCode(400);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());

  }

  @Test
  public void shouldSearchRejectionReason() {

    given(rejectionReasonRepository.searchRejectionReason(REJECTION_REASON_NAME_ONE,
            REJECTION_REASON_CODE_ONE, null)).willReturn(
            Collections.singleton(rejectionReason));

    RejectionReasonDto[] response = restAssured
        .given()
        .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
        .queryParam("name", REJECTION_REASON_NAME_ONE)
        .queryParam("code", REJECTION_REASON_CODE_ONE)
        .when()
        .get(SEARCH_URL)
        .then()
        .statusCode(200)
        .extract().as(RejectionReasonDto[].class);

    RejectionReasonDto rejectionReasonDto = response[0];
    assertEquals(REJECTION_REASON_NAME_ONE, rejectionReasonDto.getName());
    assertEquals(REJECTION_REASON_CODE_ONE, rejectionReasonDto.getCode());
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
