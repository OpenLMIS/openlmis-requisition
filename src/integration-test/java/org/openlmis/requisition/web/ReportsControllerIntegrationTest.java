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

import static org.junit.Assert.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.anySetOf;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.Collections;
import java.util.UUID;

import guru.nidi.ramltester.junit.RamlMatchers;

public class ReportsControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String PRINT_URL = "/api/requisitions/{id}/print";

  @MockBean
  private RequisitionRepository requisitionRepository;

  @MockBean
  private OrderableReferenceDataService orderableReferenceDataService;

  @Before
  public void setUp() {
    mockUserAuthenticated();
    given(orderableReferenceDataService.findByIds(anySetOf(UUID.class))).willReturn(
            Collections.emptyList());
  }

  // GET /api/requisitions/{id}/print

  @Test
  public void shouldNotPrintRequisitionWhenDoesNotExist() {
    // given
    given(requisitionRepository.findOne(anyUuid())).willReturn(null);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(APPLICATION_JSON)
        .pathParam("id", UUID.randomUUID())
        .when()
        .get(PRINT_URL)
        .then()
        .statusCode(404);

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }

  @Test
  public void shouldPrintRequisitionWhenExists() {
    // given
    Requisition requisition = generateRequisition(RequisitionStatus.AUTHORIZED);

    // when
    restAssured.given()
        .queryParam(ACCESS_TOKEN, getToken())
        .contentType(APPLICATION_JSON)
        .pathParam("id", requisition.getId())
        .when()
        .get(PRINT_URL)
        .then()
        .statusCode(200);

    // then
    assertThat(RAML_ASSERT_MESSAGE, restAssured.getLastReport(), RamlMatchers.hasNoViolations());
  }
}
