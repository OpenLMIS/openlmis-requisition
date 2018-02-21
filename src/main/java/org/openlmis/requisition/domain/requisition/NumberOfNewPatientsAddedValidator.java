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

import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.NUMBER_OF_NEW_PATIENTS_ADDED;

import lombok.AllArgsConstructor;
import org.openlmis.requisition.utils.Message;
import java.util.Map;

@AllArgsConstructor
class NumberOfNewPatientsAddedValidator implements RequisitionStatusChangeDomainValidator {
  private final Requisition requisitionToValidate;

  @Override
  public boolean isForRegularOnly() {
    return true;
  }

  @Override
  public void validateCanChangeStatus(Map<String, Message> errors) {
    requisitionToValidate.getNonSkippedFullSupplyRequisitionLineItems()
        .forEach(i -> validateFullSupplyLineItem(errors, i));
  }

  private void validateFullSupplyLineItem(Map<String, Message> errors,
                                          RequisitionLineItem item) {
    rejectIfLessThanZero(errors, requisitionToValidate.getTemplate(),
        item.getNumberOfNewPatientsAdded(), NUMBER_OF_NEW_PATIENTS_ADDED);

  }

}
