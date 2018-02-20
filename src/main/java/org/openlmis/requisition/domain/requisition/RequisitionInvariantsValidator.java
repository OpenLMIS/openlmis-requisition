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

import static org.openlmis.requisition.domain.requisition.Requisition.EMERGENCY_FIELD;
import static org.openlmis.requisition.domain.requisition.Requisition.FACILITY_ID;
import static org.openlmis.requisition.domain.requisition.Requisition.PROCESSING_PERIOD_ID;
import static org.openlmis.requisition.domain.requisition.Requisition.PROGRAM_ID;
import static org.openlmis.requisition.domain.requisition.Requisition.SUPERVISORY_NODE_ID;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_IS_INVARIANT;

import lombok.AllArgsConstructor;
import org.openlmis.requisition.utils.Message;
import java.util.Map;

@AllArgsConstructor
class RequisitionInvariantsValidator implements RequisitionDomainValidator {
  private Requisition requisitionUpdater;
  private Requisition requisitionToUpdate;

  public void validate(Map<String, Message> errors) {
    rejectIfValueChanged(errors, requisitionUpdater.getFacilityId(),
        requisitionToUpdate.getFacilityId(), FACILITY_ID);
    rejectIfValueChanged(errors, requisitionUpdater.getProgramId(),
        requisitionToUpdate.getProgramId(), PROGRAM_ID);
    rejectIfValueChanged(errors, requisitionUpdater.getProcessingPeriodId(),
        requisitionToUpdate.getProcessingPeriodId(), PROCESSING_PERIOD_ID);
    rejectIfValueChanged(errors, requisitionUpdater.getEmergency(),
        requisitionToUpdate.getEmergency(), EMERGENCY_FIELD);
    rejectIfValueChanged(errors, requisitionUpdater.getSupervisoryNodeId(),
        requisitionToUpdate.getSupervisoryNodeId(), SUPERVISORY_NODE_ID);
  }

  @Override
  public boolean isForRegularOnly() {
    return false;
  }

  private void rejectIfValueChanged(Map<String, Message> errors, Object value, Object savedValue,
                                    String field) {
    if (value != null && savedValue != null && !savedValue.equals(value)) {
      errors.put(field, new Message(ERROR_IS_INVARIANT, field));
    }
  }

}
