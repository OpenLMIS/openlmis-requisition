package org.openlmis.utils;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplate;

import java.util.List;

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
}
