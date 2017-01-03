package org.openlmis.utils;

import org.openlmis.requisition.domain.LineItemFieldsCalculator;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplate;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
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
    Set<Map<UUID, Integer>> productIdAdjustedConsumptionMapHashSet =
        getProductIdAdjustedConsumptionMapsFromRequisitions(previousRequisitions);

    forEachLineItem(requisitionLineItems, line -> {
      List<Integer> adjustedConsumptions =
          getAdjustedConsumptionListByProductId(productIdAdjustedConsumptionMapHashSet,
              line.getOrderableProductId());

      adjustedConsumptions.add(line.getAdjustedConsumption());

      int averageConsumption = getAverageConsumption(adjustedConsumptions);
      line.setAverageConsumption(averageConsumption);
    });
  }

  private static Set<Map<UUID, Integer>> getProductIdAdjustedConsumptionMapsFromRequisitions(
      List<Requisition> previousRequisitions) {
    Set<Map<UUID, Integer>> productIdAdjustedConsumptionMapHashSet = new HashSet<>();

    for (Requisition previousRequisition : previousRequisitions) {
      addProductIdAdjustedConsumptionMapFromRequisitionToSet(
          productIdAdjustedConsumptionMapHashSet, previousRequisition);
    }
    return productIdAdjustedConsumptionMapHashSet;
  }

  private static void addProductIdAdjustedConsumptionMapFromRequisitionToSet(
      Set<Map<UUID, Integer>> productIdAdjustedConsumptionMapHashSet,
      Requisition previousRequisition) {
    List<RequisitionLineItem> products = previousRequisition.getRequisitionLineItems();
    Map<UUID, Integer> productIdAdjustedConsumptionMap =
        getMapOfProductIdAndAdjustedConsumptionFromRequisitionLineItems(products);
    productIdAdjustedConsumptionMapHashSet.add(productIdAdjustedConsumptionMap);
  }

  private static Map<UUID, Integer> getMapOfProductIdAndAdjustedConsumptionFromRequisitionLineItems(
      List<RequisitionLineItem> products) {
    return products.stream()
        .collect(Collectors.toMap(RequisitionLineItem::getOrderableProductId,
            RequisitionLineItem::getAdjustedConsumption));
  }

  private static List<Integer> getAdjustedConsumptionListByProductId(
      Set<Map<UUID, Integer>> productIdAdjustedConsumptionMapHashSet, UUID productId) {
    return productIdAdjustedConsumptionMapHashSet.stream()
        .map(map -> map.get(productId))
        .collect(Collectors.toList());
  }

  private static int getAverageConsumption(List<Integer> adjustedConsumptions) {
    int[] adjustedConsumptionsArray = toArray(adjustedConsumptions);
    return LineItemFieldsCalculator
        .calculateAverageConsumption(adjustedConsumptionsArray);
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
