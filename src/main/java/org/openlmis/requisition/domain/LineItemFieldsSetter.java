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
import static org.openlmis.requisition.domain.RequisitionLineItem.CALCULATED_ORDER_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.MAXIMUM_STOCK_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL_COLUMN;

import org.openlmis.requisition.dto.StockAdjustmentReasonDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;

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

}
