package org.openlmis.requisition.domain;

import static org.apache.commons.lang.BooleanUtils.isTrue;
import static org.apache.commons.lang.StringUtils.defaultIfBlank;
import static org.openlmis.requisition.domain.AvailableRequisitionColumnOption.DEFAULT;
import static org.openlmis.requisition.domain.OpenLmisNumberUtils.zeroIfNull;
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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Collectors;

@SuppressWarnings("PMD.TooManyMethods")
public final class LineItemFieldsCalculator {
  private static final Logger LOGGER = LoggerFactory.getLogger(LineItemFieldsCalculator.class);

  private LineItemFieldsCalculator() {
  }

  /**
   * Calculates beginningBalance (A) value of current requsition line item based on previous one
   * and returns it.
   * The formula is A = E + K
   * E = Stock on Hand
   * K = Approved Quantity
   *
   * @param previous line item from previous requsition
   */
  public static int calculateBeginningBalance(RequisitionLineItem previous) {
    if (null != previous) {
      return zeroIfNull(previous.getStockOnHand())
          + zeroIfNull(previous.getApprovedQuantity());
    }
    return 0;
  }

  /**
   * Calculates TotalConsumedQuantity (C) value and returns it.
   * The formula is C = A + B (+/-) D - E
   * A = Beginning Balance
   * B = Total Received Quantity
   * C = Total Consumed Quantity
   * D = Total Losses/Adjustments
   * E = Stock on Hand
   */
  public static int calculateTotalConsumedQuantity(RequisitionLineItem lineItem) {
    return zeroIfNull(lineItem.getBeginningBalance())
        + zeroIfNull(lineItem.getTotalReceivedQuantity())
        + zeroIfNull(lineItem.getTotalLossesAndAdjustments())
        - zeroIfNull(lineItem.getStockOnHand());
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
   * Calculates Total (Y) value and returns it.
   * The formula is Y = A + B
   * A = Beginning Balance
   * B = Total Received Quantity
   */
  public static int calculateTotal(RequisitionLineItem lineItem) {
    return zeroIfNull(lineItem.getBeginningBalance())
        + zeroIfNull(lineItem.getTotalReceivedQuantity());
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
   * Calculates StockOnHand (E) value and returns it.
   * The formula is E = A + B (+/-) D - C
   * A = Beginning Balance
   * B = Total Received Quantity
   * C = Total Consumed Quantity
   * D = Total Losses/Adjustments
   * E = Stock on Hand
   */
  public static int calculateStockOnHand(RequisitionLineItem lineItem) {
    return zeroIfNull(lineItem.getBeginningBalance())
        + zeroIfNull(lineItem.getTotalReceivedQuantity())
        + zeroIfNull(lineItem.getTotalLossesAndAdjustments())
        - zeroIfNull(lineItem.getTotalConsumedQuantity());
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
   * Calculates TotalLossesAndAdjustments (D) value and returns it.
   * The property is calculated by taking all item's StockAdjustments and adding their quantities.
   * Values, whose StockAdjustmentReasons are additive, count as positive, and negative otherwise.
   */
  public static int calculateTotalLossesAndAdjustments(RequisitionLineItem lineItem,
                                                       Collection<StockAdjustmentReasonDto>
                                                           reasons) {
    int totalLossesAndAdjustments = 0;
    if (null != lineItem.getStockAdjustments()) {
      for (StockAdjustment adjustment : lineItem.getStockAdjustments()) {
        Optional<StockAdjustmentReasonDto> reason = reasons
            .stream()
            .filter(r -> r.getId().equals(adjustment.getReasonId()))
            .findFirst();

        if (reason.isPresent()) {
          int sign = isTrue(reason.get().getAdditive()) ? 1 : -1;

          totalLossesAndAdjustments += adjustment.getQuantity() * sign;
        }
      }
    }
    return totalLossesAndAdjustments;
  }

  /**
   * Sets appropriate value for Total Consumed Quantity field in {@link RequisitionLineItem}.
   */
  public static void setTotalLossesAndAdjustments(RequisitionLineItem line,
                                                  Collection<StockAdjustmentReasonDto> reasons) {
    line.setTotalLossesAndAdjustments(calculateTotalLossesAndAdjustments(line, reasons));
  }

  /**
   * Calculates the total cost of the requisition line item, by multiplying price per pack
   * and packs to ship. If either one is null, zero will be returned.
   *
   * @param lineItem the line item to calculate the value for
   * @return a {@link Money} object representing the total cost for this line
   */
  public static Money calculateTotalCost(RequisitionLineItem lineItem) {
    Money pricePerPack = lineItem.getPricePerPack();
    if (pricePerPack == null) {
      pricePerPack = new Money(RequisitionLineItem.PRICE_PER_PACK_IF_NULL);
    }

    long packsToShip = zeroIfNull(lineItem.getPacksToShip());

    return pricePerPack.mul(packsToShip);
  }

  /**
   * Calculates Adjusted Consumption (N) value and returns it.
   * The formula is N = RoundUp(C * ((M * 30) / ((M * 30) - X)))
   * C = Total Consumed Quantity
   * M = Months in the previous period
   * N = Adjusted Consumption
   * X = Total Stockout Days
   * If non-stockout days is zero the formula is N = C
   */
  public static int calculateAdjustedConsumption(RequisitionLineItem lineItem,
                                                 int monthsInThePeriod) {
    int consumedQuantity = zeroIfNull(lineItem.getTotalConsumedQuantity());

    if (consumedQuantity == 0) {
      return 0;
    }

    int totalDays = 30 * monthsInThePeriod;
    int stockoutDays = zeroIfNull(lineItem.getTotalStockoutDays());
    int nonStockoutDays = totalDays - stockoutDays;

    if (nonStockoutDays == 0) {
      return consumedQuantity;
    }

    BigDecimal divide = new BigDecimal(totalDays)
        .divide(new BigDecimal(nonStockoutDays), 1000, BigDecimal.ROUND_HALF_UP);

    BigDecimal adjustedConsumption = new BigDecimal(consumedQuantity)
        .multiply(divide)
        .setScale(0, RoundingMode.CEILING);

    return adjustedConsumption.intValue();
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
   * Calculates Average Consumption (N) value and returns it.
   * The formula is
   * P = (N<sub>t0</sub> + N<sub>t-1</sub> + N<sub>t-2</sub> + N<sub>t-(n-1)</sub>) / (n).
   * N = Adjusted Consumption.
   * P = Average Consumption.
   * n = number of periods to be averaged.
   * t = indicates relative period (t0 = current reporting period).
   * If no previous periods, and there is only t0 formula is P = N.
   * If one previous period, so t0 and t-1 formula is
   * P = Roundup( (N<sub>t0</sub> + N<sub>t-1</sub>) / 2).
   */
  public static Integer calculateAverageConsumption(List<Integer> adjustedConsumptions) {
    int numberOfPeriods = adjustedConsumptions.size();
    if (numberOfPeriods == 1) {
      return adjustedConsumptions.get(0);
    }

    if (numberOfPeriods == 2) {
      return (int) Math.ceil((adjustedConsumptions.get(0) + adjustedConsumptions.get(1)) / 2.0);
    }

    int sum = adjustedConsumptions.stream().reduce(0, Integer::sum);


    return sum / numberOfPeriods;
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

  /**
   * Calculates Maximum Stock Quantity (H) value and returns it.
   * The formula depends on selected option:
   * default => P * MaxMonthsStock
   * P = Average Consumption
   *
   * @param line     the line item to calculate the value for.
   * @param template template related with the requisition.
   * @return a {@link Integer} object representing the maximum stock quantity for this line.
   */
  public static Integer calculateMaximumStockQuantity(RequisitionLineItem line,
                                                      RequisitionTemplate template) {
    RequisitionTemplateColumn column = template
        .findColumn(RequisitionLineItem.MAXIMUM_STOCK_QUANTITY);
    AvailableRequisitionColumnOption option = column.getOption();
    String optionName = null != option
        ? defaultIfBlank(option.getOptionName(), DEFAULT)
        : DEFAULT;

    // currently we only support default option for this column. When new option will be added
    // this condition should be removed/updated
    if (!DEFAULT.equalsIgnoreCase(optionName)) {
      throw new IllegalArgumentException(
          "Unsupported option for maximum stock quantity: " + optionName
      );
    }

    int averageConsumption = zeroIfNull(line.getAverageConsumption());
    BigDecimal maxMonthsOfStock = zeroIfNull(line.getMaxMonthsOfStock());

    return BigDecimal.valueOf(averageConsumption)
        .multiply(maxMonthsOfStock)
        .setScale(0, BigDecimal.ROUND_HALF_UP)
        .intValue();
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
   * Calculates Calculated Order Quantity (I) value and returns it.
   * The formula is
   * I = H - E
   *
   * @param line the line item to calculate the value for.
   * @return a {@link Integer} object representing the Calculated Order Quantity for this line.
   */
  public static Integer calculateCalculatedOrderQuantity(RequisitionLineItem line,
                                                         RequisitionTemplate template) {
    Integer maximumStockQuantity = line.getMaximumStockQuantity();
    Integer stockOnHand = line.getStockOnHand();

    if (null == maximumStockQuantity) {
      maximumStockQuantity = calculateMaximumStockQuantity(line, template);
    }

    if (null == stockOnHand) {
      stockOnHand = calculateStockOnHand(line);
    }

    return Math.max(0, zeroIfNull(maximumStockQuantity) - zeroIfNull(stockOnHand));
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
