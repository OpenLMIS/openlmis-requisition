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

import static org.openlmis.requisition.domain.requisition.Requisition.REQUISITION_LINE_ITEMS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_ONLY_AVAILABLE_FOR_APPROVAL;
import static org.springframework.util.CollectionUtils.isEmpty;

import com.google.common.collect.Sets;
import lombok.AllArgsConstructor;
import org.openlmis.requisition.utils.Message;
import java.util.Map;
import java.util.Set;

@AllArgsConstructor
class ApprovalFieldsValidator implements RequisitionDomainValidator {
  private final Requisition requisitionUpdater;
  private final Requisition requisitionToUpdate;

  public void validate(Map<String, Message> errors) {
    if (!isEmpty(requisitionUpdater.getNonSkippedFullSupplyRequisitionLineItems())) {
      requisitionUpdater.getNonSkippedFullSupplyRequisitionLineItems()
          .forEach(i -> validateApprovalFields(errors, i));
    }
  }

  @Override
  public boolean isForRegularOnly() {
    return false;
  }

  private void validateApprovalFields(Map<String, Message> errors,
                                      RequisitionLineItem item) {
    Set<RequisitionStatus> expectedStatuses =
        Sets.newHashSet(RequisitionStatus.AUTHORIZED, RequisitionStatus.IN_APPROVAL);

    rejectIfInvalidStatusAndNotNull(errors, item.getApprovedQuantity(),
        expectedStatuses, new Message(ERROR_ONLY_AVAILABLE_FOR_APPROVAL,
            RequisitionLineItem.APPROVED_QUANTITY));

    rejectIfInvalidStatusAndNotNull(errors, item.getRemarks(),
        expectedStatuses, new Message(ERROR_ONLY_AVAILABLE_FOR_APPROVAL,
            RequisitionLineItem.REMARKS_COLUMN));
  }

  private void rejectIfInvalidStatusAndNotNull(Map<String, Message> errors,
                                               Object value,
                                               Set<RequisitionStatus> expectedStatuses,
                                               Message message) {
    if (!expectedStatuses.contains(requisitionToUpdate.getStatus()) && value != null) {
      errors.put(REQUISITION_LINE_ITEMS, message);
    }
  }

}
