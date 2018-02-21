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

import static org.openlmis.requisition.domain.requisition.LineItemFieldsCalculator.calculateStockOnHand;
import static org.openlmis.requisition.domain.requisition.Requisition.REQUISITION_LINE_ITEMS;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.STOCK_ON_HAND;
import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.TOTAL_CONSUMED_QUANTITY;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_INCORRECT_VALUE;

import lombok.AllArgsConstructor;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.utils.Message;
import java.util.Map;
import java.util.Objects;

@AllArgsConstructor
class StockOnHandValidator
    implements RequisitionUpdateDomainValidator, RequisitionStatusChangeDomainValidator {

  private final Requisition requisitionToValidate;
  private final RequisitionTemplate requisitionTemplate;

  @Override
  public boolean isForRegularOnly() {
    return true;
  }

  @Override
  public void validateCanUpdate(Map<String, Message> errors) {
    requisitionToValidate.getNonSkippedFullSupplyRequisitionLineItems()
        .forEach(i -> validateFullSupplyLineItemForUpdate(errors, i));
  }

  @Override
  public void validateCanChangeStatus(Map<String, Message> errors) {
    requisitionToValidate.getNonSkippedFullSupplyRequisitionLineItems()
        .forEach(i -> validateFullSupplyLineItem(errors, i));
  }

  private void validateFullSupplyLineItemForUpdate(Map<String, Message> errors,
                                                   RequisitionLineItem item) {
    rejectIfCalculatedAndNotNull(errors, requisitionTemplate, item.getStockOnHand(),
        RequisitionLineItem.STOCK_ON_HAND);
  }

  private void validateFullSupplyLineItem(Map<String, Message> errors,
                                          RequisitionLineItem item) {
    rejectIfNullOrNegative(errors, requisitionTemplate, item.getStockOnHand(), STOCK_ON_HAND);
    validateCalculations(errors, item);
  }

  private void validateCalculations(Map<String, Message> errors,
                                    RequisitionLineItem item) {
    boolean templateValid = requisitionTemplate.isColumnDisplayed(STOCK_ON_HAND)
        && requisitionTemplate.isColumnDisplayed(TOTAL_CONSUMED_QUANTITY);

    if (templateValid && !Objects.equals(item.getStockOnHand(), calculateStockOnHand(item))) {
      errors.put(REQUISITION_LINE_ITEMS,
          new Message(ERROR_INCORRECT_VALUE, STOCK_ON_HAND, TOTAL_CONSUMED_QUANTITY));
    }
  }


}
