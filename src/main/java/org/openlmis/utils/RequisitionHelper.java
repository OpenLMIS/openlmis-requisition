package org.openlmis.utils;

import org.openlmis.requisition.domain.LineItemFieldsCalculator;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplate;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public class RequisitionHelper {

  /**
   * Check if all required fields fot template are not filled.
   */
  public static boolean areFieldsNotFilled(RequisitionTemplate template,
                                           List<RequisitionLineItem> requisitionLineItems) {
    if (null == requisitionLineItems) {
      return false;
    }

    boolean isTotalConsumedQuantityCalculated =
        template.isColumnCalculated(Requisition.TOTAL_CONSUMED_QUANTITY);
    boolean isStockOnHandCalculated =
        template.isColumnCalculated(Requisition.STOCK_ON_HAND);

    for (RequisitionLineItem line : requisitionLineItems) {
      if (line.getSkipped()) {
        return false;
      }

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
   * Calculate Average Consumption for each line item.
   */
  public static void calculateAverageConsumption(List<Requisition> previousRequisitions,
                                                 List<RequisitionLineItem> requisitionLineItems) {
    List<RequisitionLineItem> previousRequisitionLineItems =
        getRequisitionLineItems(previousRequisitions);

    forEachLineItem(requisitionLineItems,
        line -> {
          List<RequisitionLineItem> previousRequisitionLineItemsWithOrderableProductId =
              getRequisitionLineItems(previousRequisitionLineItems, line.getOrderableProductId());
          List<Integer> adjustedConsumptions =
              mapToAdjustedConsumptions(previousRequisitionLineItemsWithOrderableProductId);
          adjustedConsumptions.add(line.getAdjustedConsumption());
          int averageConsumption =
              LineItemFieldsCalculator.calculateAverageConsumption(toArray(adjustedConsumptions));
          line.setAverageConsumption(averageConsumption);
        });
  }

  private static List<Integer> mapToAdjustedConsumptions(
      List<RequisitionLineItem> previousRequisitionLineItemsWithOrderableProductId) {
    return previousRequisitionLineItemsWithOrderableProductId
        .stream()
        .map(RequisitionLineItem::getAdjustedConsumption)
        .collect(Collectors.toList());
  }

  private static List<RequisitionLineItem> getRequisitionLineItems(
      List<RequisitionLineItem> previousRequisitionLineItemList, UUID productId) {
    return previousRequisitionLineItemList.stream()
        .filter(previousLine ->
            previousLine.getOrderableProductId().equals(productId))
        .collect(Collectors.toList());
  }

  private static List<RequisitionLineItem> getRequisitionLineItems(
      List<Requisition> previousRequisitions) {
    return previousRequisitions.stream()
        .map(Requisition::getRequisitionLineItems)
        .flatMap(Collection::stream)
        .collect(Collectors.toList());
  }

  private static int[] toArray(List<Integer> adjustedConsumptions) {
    return adjustedConsumptions.stream()
        .mapToInt(Integer::intValue)
        .toArray();
  }

  private static void forEachLineItem(List<RequisitionLineItem> requisitionLineItems,
                                      Consumer<RequisitionLineItem> consumer) {
    Optional.ofNullable(requisitionLineItems)
        .ifPresent(list -> list.forEach(consumer));
  }

}
