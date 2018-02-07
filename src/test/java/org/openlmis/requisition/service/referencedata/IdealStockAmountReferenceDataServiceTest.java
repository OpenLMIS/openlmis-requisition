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

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;
import org.openlmis.requisition.dto.IdealStockAmountDto;
import org.openlmis.requisition.service.BaseCommunicationService;
import org.openlmis.requisition.service.BaseCommunicationServiceTest;
import org.openlmis.requisition.testutils.IdealStockAmountDtoDataBuilder;
import org.openlmis.requisition.utils.DynamicPageTypeReference;
import org.springframework.http.HttpMethod;
import org.springframework.test.util.ReflectionTestUtils;
import java.util.List;
import java.util.UUID;

public class IdealStockAmountReferenceDataServiceTest
    extends BaseCommunicationServiceTest<IdealStockAmountDto> {

  private IdealStockAmountReferenceDataService service;

  @Override
  protected IdealStockAmountDto generateInstance() {
    return new IdealStockAmountDto();
  }

  @Override
  protected BaseCommunicationService<IdealStockAmountDto> getService() {
    return new IdealStockAmountReferenceDataService();
  }

  @Override
  @Before
  public void setUp() {
    super.setUp();

    service = (IdealStockAmountReferenceDataService) prepareService();
    ReflectionTestUtils.setField(service, "referenceDataUrl", "http://localhost");
  }

  @Test
  public void shouldFindIdealStockAmountBasedOnParameters() {
    IdealStockAmountDto isa = new IdealStockAmountDtoDataBuilder().build();
    mockPageResponseEntity(isa);
    UUID facilityId = UUID.randomUUID();
    UUID processingPeriodId = UUID.randomUUID();

    List<IdealStockAmountDto> result = service.search(facilityId, processingPeriodId);
    assertThat(result, hasSize(1));
    assertThat(result, hasItem(isa));

    // then
    verify(restTemplate).exchange(
        uriCaptor.capture(), eq(HttpMethod.GET),
        entityCaptor.capture(), any(DynamicPageTypeReference.class));

    String uri = uriCaptor.getValue().toString();
    String url = service.getServiceUrl() + service.getUrl();

    assertThat(uri, startsWith(url));
    assertThat(uri, containsString("facilityId=" + facilityId));
    assertThat(uri, containsString("processingPeriodId=" + processingPeriodId));

    assertAuthHeader(entityCaptor.getValue());
    assertThat(entityCaptor.getValue().getBody(), is(nullValue()));
  }
}
