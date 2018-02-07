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

package org.openlmis.requisition.service.stockmanagement;

import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.refEq;
import static org.mockito.Mockito.verify;

import org.junit.Test;
import org.openlmis.requisition.dto.stockmanagement.StockCardSummaryDto;
import org.openlmis.requisition.service.BaseCommunicationService;
import org.openlmis.requisition.utils.DynamicPageTypeReference;
import org.springframework.http.HttpMethod;
import java.net.URI;
import java.time.LocalDate;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

public class StockCardSummariesStockManagementServiceTest
    extends BaseStockmanagementServiceTest<StockCardSummaryDto> {

  private UUID programId = UUID.randomUUID();
  private UUID facilityId = UUID.randomUUID();
  private UUID orderableId = UUID.randomUUID();
  private LocalDate asOfDate = LocalDate.now();

  @Test
  public void shouldFindStockCardSummaries() {
    // given
    StockCardSummaryDto stockCardSummaryDto = mockPageResponseEntityAndGetDto();

    // when
    StockCardSummariesStockManagementService service =
        (StockCardSummariesStockManagementService) prepareService();
    List<StockCardSummaryDto> actual =
        service.search(programId, facilityId, Collections.singleton(orderableId), asOfDate);

    // then
    assertThat(actual, hasSize(1));
    assertThat(actual, contains(stockCardSummaryDto));

    verify(restTemplate).exchange(
        uriCaptor.capture(), eq(HttpMethod.GET),
        entityCaptor.capture(), refEq(new DynamicPageTypeReference<>(StockCardSummaryDto.class))
    );

    URI uri = uriCaptor.getValue();
    String url = serviceUrl + "/api/v2/stockCardSummaries";

    assertThat(uri.toString(), startsWith(url));
    assertThat(uri.toString(), containsString("programId=" + programId));
    assertThat(uri.toString(), containsString("facilityId=" + facilityId));
    assertThat(uri.toString(), containsString("orderableId=" + orderableId));
    assertThat(uri.toString(), containsString("asOfDate=" + asOfDate));

    assertAuthHeader(entityCaptor.getValue());
    assertThat(entityCaptor.getValue().getBody(), is(nullValue()));
  }

  @Override
  protected StockCardSummaryDto generateInstance() {
    return new StockCardSummaryDto();
  }

  @Override
  protected BaseCommunicationService<StockCardSummaryDto> getService() {
    return new StockCardSummariesStockManagementService();
  }

}