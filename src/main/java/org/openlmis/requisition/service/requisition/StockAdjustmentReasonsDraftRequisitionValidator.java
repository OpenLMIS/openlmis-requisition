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

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_REASON_NOT_IN_REQUISITION_REASON_LIST;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.StockAdjustmentReason;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.utils.Message;
import org.openlmis.requisition.validate.AbstractRequisitionValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Component
public class StockAdjustmentReasonsDraftRequisitionValidator extends AbstractRequisitionValidator {

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

      validateIfReasonExists(errors, requisition, savedRequisition);
    }
  }

  private void validateIfReasonExists(Errors errors, Requisition requisition,
                                      Requisition savedRequisition) {
    Set<UUID> reasons = savedRequisition.getStockAdjustmentReasons().stream()
        .map(StockAdjustmentReason::getReasonId)
        .collect(Collectors.toSet());

    requisition.getRequisitionLineItems().stream()
        .flatMap(i -> i.getStockAdjustments().stream())
        .forEach(a -> {
          if (!reasons.contains(a.getReasonId())) {
            rejectValue(errors, REQUISITION_LINE_ITEMS,
                new Message(ERROR_REASON_NOT_IN_REQUISITION_REASON_LIST, a.getReasonId()));
          }
        });
  }
}
