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

import static java.util.Collections.singletonList;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasEntry;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PRODUCTS_STOCK_CARDS_MISSING;

import java.time.LocalDate;
import java.util.Map;
import java.util.UUID;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.dto.stockmanagement.StockCardSummaryDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.service.referencedata.ApproveProductsAggregator;
import org.openlmis.requisition.testutils.ApprovedProductDtoDataBuilder;
import org.openlmis.requisition.testutils.OrderableDtoDataBuilder;
import org.openlmis.requisition.testutils.StockCardSummaryDtoDataBuilder;

@RunWith(MockitoJUnitRunner.class)
public class StandardStockOnHandRetrieverTest extends StockOnHandRetrieverTest {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Mock
  private StockCardSummariesStockManagementService stockCardSummariesStockManagementService;

  private UUID programId = UUID.randomUUID();
  private UUID facilityId = UUID.randomUUID();
  private UUID orderableId = UUID.randomUUID();
  private LocalDate asOfDate = LocalDate.now();

  private ApproveProductsAggregator products;

  private StockCardSummaryDto stockCardSummary = new StockCardSummaryDtoDataBuilder()
      .withOrderableId(orderableId)
      .build();

  @Before
  public void setUp() {
    ApprovedProductDto approvedProduct = new ApprovedProductDtoDataBuilder()
        .withOrderable(new OrderableDtoDataBuilder()
            .withId(orderableId)
            .withProgramOrderable(programId, true)
            .build())
        .build();

    products = new ApproveProductsAggregator(singletonList(approvedProduct), programId);

    when(stockCardSummariesStockManagementService
        .search(programId, facilityId, products.getFullSupplyOrderableIds(), asOfDate))
        .thenReturn(singletonList(stockCardSummary));
  }

  @Override
  StockOnHandRetriever getRetriever() {
    return new StandardStockOnHandRetriever(
        stockCardSummariesStockManagementService,
        products, programId,
        facilityId, asOfDate
    );
  }

  @Override
  void assertStockOnHands(Map<UUID, Integer> stockOnHands) {
    assertThat(stockOnHands.isEmpty(), is(false));
    assertThat(stockOnHands.size(), is(1));
    assertThat(stockOnHands, hasEntry(orderableId, stockCardSummary.getStockOnHand()));
  }

  @Test
  public void shouldThrowExceptionIfThereIsCardWithoutStockOnHand() {
    exception.expect(ValidationMessageException.class);
    exception.expectMessage(containsString(ERROR_PRODUCTS_STOCK_CARDS_MISSING));

    stockCardSummary.setStockOnHand(null);

    getRetriever().get();
  }
}
