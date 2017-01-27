package org.openlmis.utils;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplate;

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public class RequisitionHelper {

  /**
   * Check if all required fields for template are not filled.
   */
  public static boolean areFieldsNotFilled(RequisitionTemplate template,
                                           List<RequisitionLineItem> requisitionLineItems) {
    boolean isTotalConsumedQuantityCalculated =
        template.isColumnCalculated(Requisition.TOTAL_CONSUMED_QUANTITY);
    boolean isStockOnHandCalculated =
        template.isColumnCalculated(Requisition.STOCK_ON_HAND);

    for (RequisitionLineItem line : requisitionLineItems) {
      if (isTotalConsumedQuantityCalculated
          && line.allRequiredCalcFieldsNotFilled(Requisition.TOTAL_CONSUMED_QUANTITY)) {
        return true;
      }

      if (isStockOnHandCalculated
          && line.allRequiredCalcFieldsNotFilled(Requisition.STOCK_ON_HAND)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Finds {@link RequisitionLineItem} that have productId equal to the given one.
   */
  public static List<RequisitionLineItem> findByProductId(List<RequisitionLineItem> list,
                                                          UUID productId) {
    return list.stream()
        .filter(line -> productId.equals(line.getOrderableId()))
        .collect(Collectors.toList());
  }

  /**
   * Gets non skipped requisition line items from each requisition.
   */
  public static List<RequisitionLineItem> getNonSkippedLineItems(List<Requisition> list) {
    return list.stream()
        .map(Requisition::getNonSkippedRequisitionLineItems)
        .flatMap(Collection::stream)
        .collect(Collectors.toList());
  }

  /**
   * Retrieves values of Adjusted Consumption from each {@link RequisitionLineItem} and returns
   * them in the list.
   */
  public static List<Integer> mapToAdjustedConsumptions(List<RequisitionLineItem> list) {
    return list
        .stream()
        .map(RequisitionLineItem::getAdjustedConsumption)
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  /**
   * Executes the given consumer only if the given list is not null.
   */
  public static void forEachLine(List<RequisitionLineItem> items,
                                 Consumer<RequisitionLineItem> consumer) {
    Optional.ofNullable(items).ifPresent(list -> list.forEach(consumer));
  }
}
