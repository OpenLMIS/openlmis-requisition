package org.openlmis.requisition.domain;


import org.openlmis.requisition.dto.StockAdjustmentReasonDto;

import java.util.Collection;
import java.util.Optional;

import static org.apache.commons.lang.BooleanUtils.isTrue;

public class LineItemFieldsCalculator {


  /**
   * Calculates beginningBalance (A) value of current requsition line item based on previous one
   * and sets the field in this item to that value.
   * The formula is A = E + K
   * E = Stock on Hand
   * K = Approved Quantity
   *
   * @param previous line item from previous requsition
   */
  public static void calculateBeginningBalance(RequisitionLineItem current, RequisitionLineItem
                                                  previous) {

    Integer beginningBalance = 0;
    if (null != previous) {
      beginningBalance = zeroIfNull(previous.getStockOnHand())
          + zeroIfNull(previous.getApprovedQuantity());
    }
    current.setBeginningBalance(beginningBalance);
  }

  /**
   * Calculates TotalConsumedQuantity (C) value and sets the field in this item to that value.
   * The formula is C = A + B (+/-) D - E
   * A = Beginning Balance
   * B = Total Received Quantity
   * C = Total Consumed Quantity
   * D = Total Losses/Adjustments
   * E = Stock on Hand
   */
  public static void calculateTotalConsumedQuantity(RequisitionLineItem lineItem) {
    Integer totalConsumedQuantity = zeroIfNull(lineItem.getBeginningBalance())
        + zeroIfNull(lineItem.getTotalReceivedQuantity())
        + zeroIfNull(lineItem.getTotalLossesAndAdjustments())
        - zeroIfNull(lineItem.getStockOnHand());
    lineItem.setTotalConsumedQuantity(totalConsumedQuantity);
  }

  /**
   * Calculates Total (Y) value and sets the field in this item to that value.
   * The formula is Y = A + B
   * A = Beginning Balance
   * B = Total Received Quantity
   */
  public static void calculateTotal(RequisitionLineItem lineItem) {
    Integer total = zeroIfNull(lineItem.getBeginningBalance())
        + zeroIfNull(lineItem.getTotalReceivedQuantity());
    lineItem.setTotal(total);
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
  public static Integer calculateStockOnHandValue(RequisitionLineItem lineItem) {
    Integer stockOnHand =  zeroIfNull(lineItem.getBeginningBalance())
        + zeroIfNull(lineItem.getTotalReceivedQuantity())
        + zeroIfNull(lineItem.getTotalLossesAndAdjustments())
        - zeroIfNull(lineItem.getTotalConsumedQuantity());
    return stockOnHand;
  }

  /**
   * Calculates StockOnHand (E) value and sets the field in this item to that value.
   */
  public static void calculateStockOnHand(RequisitionLineItem lineItem) {
    lineItem.setStockOnHand(calculateStockOnHandValue(lineItem));
  }

  /**
   * Calculates TotalLossesAndAdjustments (D) value and sets the field in this item to that value.
   * The property is calculated by taking all item's StockAdjustments and adding their quantities.
   * Values, whose StockAdjustmentReasons are additive, count as positive, and negative otherwise.
   */
  public static void calculateTotalLossesAndAdjustments(RequisitionLineItem lineItem,
                                                               Collection<StockAdjustmentReasonDto>
                                                               reasons) {
    Integer totalLossesAndAdjustments = 0;
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
    lineItem.setTotalLossesAndAdjustments(totalLossesAndAdjustments);
  }

  private static int zeroIfNull(Integer value) {
    return null == value ? 0 : value;
  }

}
