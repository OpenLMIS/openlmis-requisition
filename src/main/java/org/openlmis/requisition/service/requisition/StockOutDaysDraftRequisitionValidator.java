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

package org.openlmis.requisition.service.requisition;

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_STOCKOUT_DAYS_CANT_BE_GREATER_THAN_LENGTH_OF_PERIOD;
import static org.springframework.util.CollectionUtils.isEmpty;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.validate.AbstractRequisitionValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
public class StockOutDaysDraftRequisitionValidator extends AbstractRequisitionValidator {

  private static final int DAYS_IN_MONTH = 30;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Override
  public boolean supports(Class<?> clazz) {
    return Requisition.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    Requisition requisition = (Requisition) target;
    if (!requisition.getEmergency()) {
      Requisition savedRequisition = requisitionRepository.findOne(requisition.getId());

      if (!isEmpty(requisition.getNonSkippedFullSupplyRequisitionLineItems())) {
        requisition.getNonSkippedFullSupplyRequisitionLineItems()
            .forEach(i -> rejectIfTotalStockOutDaysIsGreaterThanLengthOfPeriod(
                errors, savedRequisition.getNumberOfMonthsInPeriod(), i));
      }
    }
  }

  private void rejectIfTotalStockOutDaysIsGreaterThanLengthOfPeriod(
      Errors errors, int monthsInPeriod, RequisitionLineItem requisitionLineItem) {
    if (requisitionLineItem.getTotalStockoutDays() == null) {
      return;
    }
    if (requisitionLineItem.getTotalStockoutDays() > monthsInPeriod * DAYS_IN_MONTH) {
      rejectValue(errors, REQUISITION_LINE_ITEMS,
          new Message(ERROR_STOCKOUT_DAYS_CANT_BE_GREATER_THAN_LENGTH_OF_PERIOD));
    }
  }

}
