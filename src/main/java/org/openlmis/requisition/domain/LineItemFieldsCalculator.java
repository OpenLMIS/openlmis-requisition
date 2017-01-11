package org.openlmis.requisition.domain;

import static org.apache.commons.lang.BooleanUtils.isTrue;

import org.openlmis.requisition.dto.StockAdjustmentReasonDto;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

@SuppressWarnings("PMD.TooManyMethods")
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

    BigDecimal adjustedConsumption = new BigDecimal(consumedQuantity)
        .multiply(divide(totalDays, nonStockoutDays))
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

  private static BigDecimal divide(int totalDays, int nonStockoutDays) {
    return new BigDecimal(totalDays)
        .divide(new BigDecimal(nonStockoutDays), 1000, BigDecimal.ROUND_HALF_UP);
  }

  private static int zeroIfNull(Integer value) {
    return null == value ? 0 : value;
  }

  private static long zeroIfNull(Long value) {
    return null == value ? 0 : value;
  }
}
