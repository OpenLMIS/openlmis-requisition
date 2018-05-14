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

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_PRODUCTS_STOCK_CARDS_MISSING;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import lombok.AllArgsConstructor;
import org.openlmis.requisition.dto.stockmanagement.StockCardSummaryDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.service.referencedata.ApproveProductsAggregator;

@AllArgsConstructor
final class StandardStockOnHandRetriever implements StockOnHandRetriever {
  private StockCardSummariesStockManagementService stockCardSummariesService;
  private ApproveProductsAggregator products;
  private UUID programId;
  private UUID facilityId;
  private LocalDate asOfDate;

  public Map<UUID, Integer> get() {
    List<StockCardSummaryDto> cards = getCards();
    validate(cards);
    return convert(cards);
  }

  private List<StockCardSummaryDto> getCards() {
    return stockCardSummariesService
        .search(programId, facilityId, products.getFullSupplyOrderableIds(), asOfDate);
  }

  private void validate(List<StockCardSummaryDto> cards) {
    List<StockCardSummaryDto> cardsWithout = getCardsWithoutStockOnHand(cards);

    if (cardsWithout.isEmpty()) {
      return;
    }

    String firstProductName = getFirstProductName(cardsWithout);

    throw new ValidationMessageException(
        ERROR_PRODUCTS_STOCK_CARDS_MISSING,
        firstProductName, cardsWithout.size()
    );
  }

  private List<StockCardSummaryDto> getCardsWithoutStockOnHand(List<StockCardSummaryDto> cards) {
    return cards
        .stream()
        .filter(c -> null == c.getStockOnHand())
        .collect(Collectors.toList());
  }

  private String getFirstProductName(List<StockCardSummaryDto> cardsWithout) {
    return products
        .getFullSupplyProduct(cardsWithout.get(0).getOrderable().getId())
        .getOrderable()
        .getFullProductName();
  }

  private Map<UUID, Integer> convert(List<StockCardSummaryDto> cards) {
    return cards
        .stream()
        .collect(Collectors.toMap(
            card -> card.getOrderable().getId(),
            StockCardSummaryDto::getStockOnHand
        ));
  }

}
