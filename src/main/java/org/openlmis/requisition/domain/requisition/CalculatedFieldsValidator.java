/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org.
 */

package org.openlmis.requisition.domain.requisition;

import static org.openlmis.requisition.domain.requisition.LineItemFieldsCalculator.calculateCalculatedOrderQuantity;
import static org.openlmis.requisition.domain.requisition.LineItemFieldsCalculator.calculateCalculatedOrderQuantityIsa;
import static org.openlmis.requisition.domain.requisition.LineItemFieldsCalculator.calculateMaximumStockQuantity;
import static org.openlmis.requisition.domain.requisition.Requisition.REQUISITION_LINE_ITEMS;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.CALCULATED_ORDER_QUANTITY;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.CALCULATED_ORDER_QUANTITY_ISA;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.MAXIMUM_STOCK_QUANTITY;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALUE_DOES_NOT_MATCH_CALCULATED_VALUE;

import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.utils.Message;
import java.util.Map;
import java.util.Objects;

class CalculatedFieldsValidator
    extends AbstractRegularRequisitionFullSupplyLineItemStatusChangeValidator {

  private final RequisitionTemplate requisitionTemplate;


  CalculatedFieldsValidator(Requisition requisitionToValidate,
                            RequisitionTemplate requisitionTemplate) {
    super(requisitionToValidate);
    this.requisitionTemplate = requisitionTemplate;
  }

  protected void validateFullSupplyLineItem(Map<String, Message> errors,
                                          RequisitionLineItem item) {
    validateMaximumStockQuantity(errors, item);
    validateCalculatedOrderQuantity(errors, item);
    validateCalculatedOrderQuantityIsa(errors, item);
  }

  private void validateCalculatedOrderQuantity(Map<String, Message> errors,
                                               RequisitionLineItem item) {
    boolean coqDisplayed = isColumnDisplayed(CALCULATED_ORDER_QUANTITY);
    rejectIfNonNullValueForHiddenColumn(errors, item.getCalculatedOrderQuantity(),
        CALCULATED_ORDER_QUANTITY, coqDisplayed);
    if (coqDisplayed && calculatedOrderQuantityDoesNotMatchCalculatedValue(item)) {
      rejectValueDoesNotMatchCalculations(errors, CALCULATED_ORDER_QUANTITY);
    }
  }

  private void validateCalculatedOrderQuantityIsa(Map<String, Message> errors,
                                                  RequisitionLineItem item) {
    if (requisitionTemplate.isColumnInTemplate(CALCULATED_ORDER_QUANTITY_ISA)) {
      boolean coqIsaDisplayed = isColumnDisplayed(CALCULATED_ORDER_QUANTITY_ISA);
      rejectIfNonNullValueForHiddenColumn(errors, item.getCalculatedOrderQuantityIsa(),
          CALCULATED_ORDER_QUANTITY_ISA, coqIsaDisplayed);
      if (coqIsaDisplayed && calculatedOrderQuantityIsaDoesNotMatchCalculatedValue(item)) {
        rejectValueDoesNotMatchCalculations(errors, CALCULATED_ORDER_QUANTITY_ISA);
      }
    }
  }

  private void validateMaximumStockQuantity(Map<String, Message> errors,
                                            RequisitionLineItem item) {
    boolean msqDisplayed = isColumnDisplayed(MAXIMUM_STOCK_QUANTITY);
    rejectIfNonNullValueForHiddenColumn(errors, item.getMaximumStockQuantity(),
        MAXIMUM_STOCK_QUANTITY, msqDisplayed);
    if (msqDisplayed && maximumStockQuantityDoesNotMatchCalculatedValue(item)) {
      rejectValueDoesNotMatchCalculations(errors, MAXIMUM_STOCK_QUANTITY);
    }
  }

  private boolean isColumnDisplayed(String maximumStockQuantity) {
    return requisitionTemplate.isColumnDisplayed(maximumStockQuantity);
  }

  private boolean calculatedOrderQuantityDoesNotMatchCalculatedValue(RequisitionLineItem item) {
    return !Objects.equals(item.getCalculatedOrderQuantity(),
        calculateCalculatedOrderQuantity(item, requisitionTemplate));
  }

  private boolean calculatedOrderQuantityIsaDoesNotMatchCalculatedValue(RequisitionLineItem item) {
    return !Objects.equals(item.getCalculatedOrderQuantityIsa(),
        calculateCalculatedOrderQuantityIsa(item));
  }

  private boolean maximumStockQuantityDoesNotMatchCalculatedValue(RequisitionLineItem item) {
    return !Objects.equals(item.getMaximumStockQuantity(),
        calculateMaximumStockQuantity(item, requisitionTemplate));
  }

  private void rejectValueDoesNotMatchCalculations(Map<String, Message> errors, String field) {
    errors.put(REQUISITION_LINE_ITEMS,
        new Message(ERROR_VALUE_DOES_NOT_MATCH_CALCULATED_VALUE, field));
  }

}
