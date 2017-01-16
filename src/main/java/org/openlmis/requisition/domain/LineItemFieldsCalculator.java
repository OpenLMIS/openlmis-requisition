package org.openlmis.requisition.domain;

import static org.apache.commons.lang.BooleanUtils.isTrue;
import static org.apache.commons.lang.StringUtils.defaultIfBlank;
import static org.openlmis.requisition.domain.AvailableRequisitionColumnOption.DEFAULT;
import static org.openlmis.requisition.domain.OpenLmisNumberUtils.zeroIfNull;

import org.openlmis.requisition.dto.StockAdjustmentReasonDto;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

public final class LineItemFieldsCalculator {

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
  public static int calculateAverageConsumption(List<Integer> adjustedConsumptions) {
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
   * Calculates Maximum Stock Quantity (H) value and returns it.
   * The formula depends on selected option:
   * default => P * MaxMonthsStock
   * P = Average Consumption
   *
   * @param line     the line item to calculate the value for.
   * @param template template related with the requisition.
   * @return a {@link Integer} object representing the maximum stock quantity for this line.
   */
  public static int calculateMaximumStockQuantity(RequisitionLineItem line,
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
   * Calculates Calculated Order Quantity (I) value and returns it.
   * The formula is
   * I = H - E
   *
   * @param line the line item to calculate the value for.
   * @return a {@link Integer} object representing the Calculated Order Quantity for this line.
   */
  public static int calculateCalculatedOrderQuantity(RequisitionLineItem line,
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

}
