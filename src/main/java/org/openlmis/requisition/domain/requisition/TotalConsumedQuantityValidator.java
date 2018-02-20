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

import static org.springframework.util.CollectionUtils.isEmpty;

import lombok.AllArgsConstructor;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.utils.Message;
import java.util.Map;

@AllArgsConstructor
class TotalConsumedQuantityValidator implements RequisitionDomainValidator {

  private final Requisition requisition;
  private final RequisitionTemplate requisitionTemplate;

  @Override
  public void validate(Map<String, Message> errors) {

    if (!isEmpty(requisition.getNonSkippedFullSupplyRequisitionLineItems())) {
      requisition.getNonSkippedFullSupplyRequisitionLineItems()
          .forEach(i -> validateRequisitionLineItem(errors, i));
    }
  }

  @Override
  public boolean isForRegularOnly() {
    return true;
  }

  private void validateRequisitionLineItem(Map<String, Message> errors,
                                           RequisitionLineItem item) {
    rejectIfCalculatedAndNotNull(errors, requisitionTemplate, item.getTotalConsumedQuantity(),
        RequisitionLineItem.TOTAL_CONSUMED_QUANTITY);
  }
}
