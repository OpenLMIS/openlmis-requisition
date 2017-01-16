package org.openlmis.requisition.domain;

import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateAdjustedConsumption;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateCalculatedOrderQuantity;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateMaximumStockQuantity;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateStockOnHand;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateTotal;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateTotalConsumedQuantity;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateTotalLossesAndAdjustments;
import static org.openlmis.requisition.domain.Requisition.STOCK_ON_HAND;
import static org.openlmis.requisition.domain.Requisition.TOTAL_CONSUMED_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.ADJUSTED_CONSUMPTION;
import static org.openlmis.requisition.domain.RequisitionLineItem.AVERAGE_CONSUMPTION;
import static org.openlmis.requisition.domain.RequisitionLineItem.CALCULATED_ORDER_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.MAXIMUM_STOCK_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL_COLUMN;

import org.openlmis.requisition.dto.StockAdjustmentReasonDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public final class LineItemFieldsSetter {
  private static final Logger LOGGER = LoggerFactory.getLogger(LineItemFieldsSetter.class);

  private LineItemFieldsSetter() {
    throw new UnsupportedOperationException();
  }

  /**
   * Sets appropriate value for Total Consumed Quantity field in {@link RequisitionLineItem}.
   */
  public static void setTotalConsumedQuantity(RequisitionTemplate template,
                                              RequisitionLineItem line) {
    if (template.isColumnDisplayed(TOTAL_CONSUMED_QUANTITY)) {
      if (template.isColumnCalculated(TOTAL_CONSUMED_QUANTITY)) {
        line.setTotalConsumedQuantity(calculateTotalConsumedQuantity(line));
      }
    } else {
      line.setTotalConsumedQuantity(null);
    }
  }

  /**
   * Sets appropriate value for Total field in {@link RequisitionLineItem}.
   */
  public static void setTotal(RequisitionTemplate template, RequisitionLineItem line) {
    if (template.isColumnDisplayed(TOTAL_COLUMN)) {
      line.setTotal(calculateTotal(line));
    }
  }

  /**
   * Sets appropriate value for Stock On Hand field in {@link RequisitionLineItem}.
   */
  public static void setStockOnHand(RequisitionTemplate template, RequisitionLineItem line) {
    if (template.isColumnDisplayed(STOCK_ON_HAND)) {
      if (template.isColumnCalculated(STOCK_ON_HAND)) {
        line.setStockOnHand(calculateStockOnHand(line));
      }
    } else {
      line.setStockOnHand(null);
    }
  }

  /**
   * Sets appropriate value for Total Consumed Quantity field in {@link RequisitionLineItem}.
   */
  public static void setTotalLossesAndAdjustments(RequisitionLineItem line,
                                                  Collection<StockAdjustmentReasonDto> reasons) {
    line.setTotalLossesAndAdjustments(calculateTotalLossesAndAdjustments(line, reasons));
  }

  /**
   * Sets appropriate value for Adjusted Consumption field in {@link RequisitionLineItem}.
   */
  public static void setAdjustedConsumption(RequisitionTemplate template,
                                            RequisitionLineItem line,
                                            Integer monthsInThePeriod) {
    if (template.isColumnInTemplate(ADJUSTED_CONSUMPTION)) {
      int adjustedConsumption = calculateAdjustedConsumption(line, monthsInThePeriod);
      if (line.getAdjustedConsumption() != null
          && adjustedConsumption != line.getAdjustedConsumption()) {
        LOGGER.warn("Passed Adjusted Consumption does not match calculated one.");
      }
      line.setAdjustedConsumption(adjustedConsumption);
    }
  }

  /**
   * Sets appropriate value for Maximum Stock Quantity field in {@link RequisitionLineItem}.
   */
  public static void setMaximumStockQuantity(RequisitionTemplate template,
                                             RequisitionLineItem line) {
    if (template.isColumnDisplayed(MAXIMUM_STOCK_QUANTITY)) {
      line.setMaximumStockQuantity(calculateMaximumStockQuantity(line, template));
    }
  }

  /**
   * Sets appropriate value for Calculated Order Quantity field in {@link RequisitionLineItem}.
   */
  public static void setCalculatedOrderQuantity(RequisitionTemplate template,
                                                RequisitionLineItem line) {
    if (template.isColumnDisplayed(CALCULATED_ORDER_QUANTITY)) {
      line.setCalculatedOrderQuantity(calculateCalculatedOrderQuantity(line, template));
    }
  }

  /**
   * Sets appropriate value for Adjusted Consumption field in {@link RequisitionLineItem} on update.
   */
  public static void setAverageConsumptionOnUpdate(RequisitionTemplate template,
                                                   RequisitionLineItem line) {
    if (template.isColumnInTemplate(AVERAGE_CONSUMPTION)) {
      Integer averageConsumptionPassed = line.getAverageConsumption();
      setAverageConsumption(line);
      if (averageConsumptionPassed != null
          && !Objects.equals(averageConsumptionPassed, line.getAdjustedConsumption())) {
        LOGGER.warn("Passed Average Consumption does not match calculated one.");
      }
    }
  }

  /**
   * Sets appropriate value for Average Consumption field in {@link RequisitionLineItem}.
   */
  public static void setAverageConsumption(RequisitionLineItem requisitionLineItem) {

    List<Integer> previousAdjustedConsumptions =
        requisitionLineItem.getPreviousAdjustedConsumptions();
    previousAdjustedConsumptions.add(requisitionLineItem.getAdjustedConsumption());
    Integer averageConsumption =
        LineItemFieldsCalculator.calculateAverageConsumption(previousAdjustedConsumptions);
    requisitionLineItem.setAverageConsumption(averageConsumption);
  }

  /**
   * Set previous adjusted consumptions to requisitionLineItems.
   */
  public static void setPreviousAdjustedConsumptions(List<RequisitionLineItem> requisitionLineItems,
                                                     List<Requisition> previousRequisitions) {
    List<RequisitionLineItem> previousRequisitionLineItems =
        getRequisitionLineItems(previousRequisitions);

    forEachLineItem(requisitionLineItems,
        line -> {
          List<RequisitionLineItem> previousRequisitionLineItemsWithOrderableProductId =
              getRequisitionLineItems(previousRequisitionLineItems, line.getOrderableProductId());
          List<Integer> adjustedConsumptions =
              mapToAdjustedConsumptions(previousRequisitionLineItemsWithOrderableProductId);
          adjustedConsumptions = adjustedConsumptions.stream()
              .filter(adjustedConsumption -> adjustedConsumption != null)
              .collect(Collectors.toList());

          line.setPreviousAdjustedConsumptions(adjustedConsumptions);
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
        .map(Requisition::getNonSkippedRequisitionLineItems)
        .flatMap(Collection::stream)
        .collect(Collectors.toList());
  }

  private static void forEachLineItem(List<RequisitionLineItem> requisitionLineItems,
                                      Consumer<RequisitionLineItem> consumer) {
    Optional.ofNullable(requisitionLineItems)
        .ifPresent(list -> list.forEach(consumer));
  }
}
