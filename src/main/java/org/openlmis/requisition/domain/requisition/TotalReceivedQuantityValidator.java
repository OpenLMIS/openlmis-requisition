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

import static org.openlmis.requisition.domain.requisition.RequisitionLineItem.TOTAL_RECEIVED_QUANTITY;

import java.util.Map;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.VersionIdentityDto;
import org.openlmis.requisition.utils.Message;

class TotalReceivedQuantityValidator
    extends AbstractRegularRequisitionFullSupplyLineItemStatusChangeValidator {

  private final RequisitionTemplate requisitionTemplate;


  TotalReceivedQuantityValidator(Requisition requisitionToValidate,
      RequisitionTemplate requisitionTemplate, Map<VersionIdentityDto, OrderableDto> orderables) {
    super(requisitionToValidate, orderables);
    this.requisitionTemplate = requisitionTemplate;
  }

  @Override
  protected void validateFullSupplyLineItem(Map<String, Message> errors, RequisitionLineItem item) {
    boolean skipValidation = !requisitionToValidate.getStatus().isAuthorized()
        && requisitionTemplate.isPatientsTabEnabled();
    if (!skipValidation) {
      rejectIfNullOrNegative(errors, requisitionTemplate,
          item.getTotalReceivedQuantity(), TOTAL_RECEIVED_QUANTITY);
    }
  }

  @Override
  public boolean isForApprove() {
    return false;
  }
}
