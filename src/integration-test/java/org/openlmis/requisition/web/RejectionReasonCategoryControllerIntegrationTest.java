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

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

import com.google.common.collect.Sets;
import guru.nidi.ramltester.junit.RamlMatchers;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import org.junit.Test;
import org.openlmis.requisition.domain.RejectionReasonCategory;
import org.openlmis.requisition.dto.RejectionReasonCategoryDto;
import org.openlmis.requisition.testutils.RejectionReasonCategoryDataBuilder;
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

  /**
   * Constructor for test class.
   */
  public RejectionReasonCategoryControllerIntegrationTest() {
    rejectionReasonCategory1 = new RejectionReasonCategoryDataBuilder()
            .withCode(REJECTION_REASON_CATEGORY_CODE_ONE)
            .withName(REJECTION_REASON_CATEGORY_NAME_ONE)
            .buildAsNew();

    rejectionReasonCategoryDto = new RejectionReasonCategoryDto();
    rejectionReasonCategory1.export(rejectionReasonCategoryDto);
    rejectionReasonCategoryId = UUID.randomUUID();
  }

  @Test
  public void getAllShouldGetAllRejectionReasonCategories() {

    Set<RejectionReasonCategory> storedRejectionReasonCategories =
            Sets.newHashSet(rejectionReasonCategory1,
                    RejectionReasonCategory.newRejectionReasonCategory(
                            "rejectionReasonCategory2", "RRC4"
                    ));
    given(rejectionReasonCategoryRepository.findAll()).willReturn(storedRejectionReasonCategories);

    RejectionReasonCategoryDto[] response = restAssured
            .given()
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .when()
            .get(RESOURCE_URL)
            .then()
            .statusCode(200)
            .extract().as(RejectionReasonCategoryDto[].class);

    List<RejectionReasonCategoryDto> rejectionReasonCategories = Arrays.asList(response);
    assertThat(rejectionReasonCategories.size(), is(2));
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void getShouldGetRejectionReasonCategory() {

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

    given(rejectionReasonCategoryRepository.findById(rejectionReasonCategoryId))
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
  public void putShouldCreateNewRejectionReasonCategoryForNonExistingRejectionReasonCategory() {
    given(rejectionReasonCategoryRepository.findFirstByName(REJECTION_REASON_CATEGORY_NAME_ONE))
            .willReturn(null);
    given(rejectionReasonCategoryRepository.save(any()))
            .willReturn(rejectionReasonCategory1);

    RejectionReasonCategoryDto response = restAssured
            .given()
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .body(rejectionReasonCategoryDto)
            .when()
            .put(RESOURCE_URL)
            .then()
            .statusCode(200)
            .extract().as(RejectionReasonCategoryDto.class);

    assertEquals(REJECTION_REASON_CATEGORY_NAME_ONE, response.getName());
    assertEquals(REJECTION_REASON_CATEGORY_CODE_ONE, response.getCode());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void putShouldUpdateRejectionReasonCategoryForExistingRejectionReasonCategory() {
    given(rejectionReasonCategoryRepository.findFirstByName(REJECTION_REASON_CATEGORY_NAME_ONE))
            .willReturn(rejectionReasonCategory1);
    given(rejectionReasonCategoryRepository.save(any()))
            .willReturn(rejectionReasonCategory1);

    RejectionReasonCategoryDto response = restAssured
            .given()
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .body(rejectionReasonCategoryDto)
            .when()
            .put(RESOURCE_URL)
            .then()
            .statusCode(200)
            .extract().as(RejectionReasonCategoryDto.class);

    assertEquals(REJECTION_REASON_CATEGORY_NAME_ONE, response.getName());
    assertEquals(REJECTION_REASON_CATEGORY_CODE_ONE, response.getCode());
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void deleteShouldDeleteRejectionReasonCategory() {

    given(rejectionReasonCategoryRepository.findById(rejectionReasonCategoryId))
            .willReturn(Optional.of(rejectionReasonCategory1));

    restAssured
            .given()
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .pathParam("id", rejectionReasonCategoryId)
            .when()
            .delete(ID_URL)
            .then()
            .statusCode(204);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void deleteShouldReturnNotFoundForNonExistingRejectionReasonCategory() {

    given(rejectionReasonCategoryRepository.findById(rejectionReasonCategoryId))
            .willReturn(Optional.empty());

    restAssured
            .given()
            .header(HttpHeaders.AUTHORIZATION, getTokenHeader())
            .pathParam("id", rejectionReasonCategoryId)
            .when()
            .delete(ID_URL)
            .then()
            .statusCode(404);

    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void searchShouldFindRejectionReasonByNameAndCode() {

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

}
