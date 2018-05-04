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
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.powermock.api.mockito.PowerMockito.doReturn;

import guru.nidi.ramltester.junit.RamlMatchers;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.repository.AvailableRequisitionColumnRepository;
import org.openlmis.requisition.service.PageDto;
import org.openlmis.requisition.testutils.AvailableRequisitionColumnDataBuilder;
import org.openlmis.requisition.utils.Pagination;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpHeaders;

public class AvailableRequisitionColumnControllerIntegrationTest extends BaseWebIntegrationTest {

  private static final String RESOURCE_URL = "/api/availableRequisitionColumns";

  @MockBean
  private AvailableRequisitionColumnRepository repository;

  private AvailableRequisitionColumn column;
  private Pageable pageable;

  @Before
  public void setUp() {
    mockUserAuthenticated();

    column = new AvailableRequisitionColumnDataBuilder().build();
    pageable = new PageRequest(0, 10);
  }

  @Test
  public void shouldReturnPageOfAvailableRequisitionColumns() {
    doReturn(Pagination.getPage(singletonList(column), pageable))
        .when(repository).findAll(pageable);

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
}
