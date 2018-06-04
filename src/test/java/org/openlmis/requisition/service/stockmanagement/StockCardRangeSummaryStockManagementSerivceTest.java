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

import java.net.URI;
import java.time.LocalDate;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import org.junit.Test;
import org.openlmis.requisition.dto.StockCardRangeSummaryDto;
import org.openlmis.requisition.service.BaseCommunicationService;
import org.openlmis.requisition.utils.DynamicPageTypeReference;
import org.springframework.http.HttpMethod;

public class StockCardRangeSummaryStockManagementSerivceTest
    extends BaseStockmanagementServiceTest<StockCardRangeSummaryDto> {

  private UUID programId = UUID.randomUUID();
  private UUID facilityId = UUID.randomUUID();
  private UUID orderableId = UUID.randomUUID();
  private String tag = "tag";
  private LocalDate startDate = LocalDate.of(2017, 1, 1);
  private LocalDate endDate = LocalDate.now();

  @Test
  public void shouldFindStockCardSummaries() {
    // given
    StockCardRangeSummaryDto stockCardRangeSummaryDto = mockPageResponseEntityAndGetDto();

    // when
    StockCardRangeSummaryStockManagementService service =
        (StockCardRangeSummaryStockManagementService) prepareService();
    List<StockCardRangeSummaryDto> actual = service.search(
        programId, facilityId, Collections.singleton(orderableId), tag, startDate, endDate);

    // then
    assertThat(actual, hasSize(1));
    assertThat(actual, contains(stockCardRangeSummaryDto));

    verify(restTemplate).exchange(
        uriCaptor.capture(), eq(HttpMethod.GET),
        entityCaptor.capture(), refEq(
            new DynamicPageTypeReference<>(StockCardRangeSummaryDto.class))
    );

    URI uri = uriCaptor.getValue();
    String url = serviceUrl + "/api/stockCardRangeSummaries";

    assertThat(uri.toString(), startsWith(url));
    assertThat(uri.toString(), containsString("programId=" + programId));
    assertThat(uri.toString(), containsString("facilityId=" + facilityId));
    assertThat(uri.toString(), containsString("orderableId=" + orderableId));
    assertThat(uri.toString(), containsString("tag=" + tag));
    assertThat(uri.toString(), containsString("startDate=" + startDate));
    assertThat(uri.toString(), containsString("endDate=" + endDate));

    assertAuthHeader(entityCaptor.getValue());
    assertThat(entityCaptor.getValue().getBody(), is(nullValue()));
  }

  @Override
  protected StockCardRangeSummaryDto generateInstance() {
    return new StockCardRangeSummaryDto();
  }

  @Override
  protected BaseCommunicationService<StockCardRangeSummaryDto> getService() {
    return new StockCardRangeSummaryStockManagementService();
  }
}
