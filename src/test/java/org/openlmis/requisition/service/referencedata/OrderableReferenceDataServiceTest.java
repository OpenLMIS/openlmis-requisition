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

package org.openlmis.requisition.service.referencedata;

import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.Collections;
import java.util.List;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.testutils.OrderableDtoDataBuilder;

public class OrderableReferenceDataServiceTest extends BaseReferenceDataServiceTest<OrderableDto> {

  private OrderableReferenceDataService service;

  @Override
  protected BaseReferenceDataService<OrderableDto> getService() {
    return new OrderableReferenceDataService();
  }

  @Override
  protected OrderableDto generateInstance() {
    return new OrderableDtoDataBuilder().buildAsDto();
  }

  @Override
  @Before
  public void setUp() {
    super.setUp();

    service = (OrderableReferenceDataService) prepareService();
  }

  @Test
  public void shouldReturnOrderablesById() {
    // given
    UUID orderableId = UUID.randomUUID();

    // when
    OrderableDto product = mockPageResponseEntityAndGetDto();
    List<OrderableDto> response = service.findByIds(Collections.singleton(orderableId));

    // then
    assertThat(response, hasSize(1));
    assertThat(response, hasItem(product));

    verifyPageRequest()
        .isGetRequest()
        .hasAuthHeader()
        .hasEmptyBody()
        .isUriStartsWith(service.getServiceUrl() + service.getUrl())
        .hasQueryParameter("id", orderableId);
  }

  @Test
  public void shouldReturnEmptyListIfEmptyParamProvided() {
    // given
    disableAuthCheck();

    // when
    List<OrderableDto> response = service.findByIds(Collections.emptyList());

    // then
    assertTrue(response.isEmpty());
  }

  @Test
  public void shouldFindAllResources() {
    // when
    OrderableDto dto = mockPageResponseEntityAndGetDto();
    List<OrderableDto> found = service.findAll();

    // then
    assertThat(found, hasItem(dto));

    verifyPageRequest()
        .isGetRequest()
        .hasAuthHeader()
        .hasEmptyBody()
        .isUriStartsWith(service.getServiceUrl() + service.getUrl());
  }
}
