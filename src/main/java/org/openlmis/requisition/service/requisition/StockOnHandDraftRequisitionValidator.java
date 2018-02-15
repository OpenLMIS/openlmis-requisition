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

import static org.springframework.util.CollectionUtils.isEmpty;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.validate.AbstractRequisitionValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
public class StockOnHandDraftRequisitionValidator extends AbstractRequisitionValidator {

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
            .forEach(i -> validateRequisitionLineItem(errors, savedRequisition, i));
      }
    }
  }

  private void validateRequisitionLineItem(Errors errors, Requisition requisition,
                                           RequisitionLineItem item) {
    RequisitionTemplate template = requisition.getTemplate();
    rejectIfCalculatedAndNotNull(errors, template, item.getStockOnHand(),
        RequisitionLineItem.STOCK_ON_HAND);
  }

}
