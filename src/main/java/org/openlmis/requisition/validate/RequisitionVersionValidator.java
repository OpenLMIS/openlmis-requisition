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

package org.openlmis.requisition.validate;

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DATE_MODIFIED_MISMATCH;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.errorhandling.ValidationResult;
import org.springframework.stereotype.Component;

import java.time.ZonedDateTime;

/**
 * Validates the date modified of a requisition against an incoming object.
 * If the date modified in the incoming object is present and does not match
 * the current one, the validation should lead to a 409 error. If the timestamp
 * is not part of the incoming object, we can skip it.
 */
@Component
public class RequisitionVersionValidator {

  /**
   * Validates if the date modified of the incoming requisition matches
   * the date modified of the existing requisition. If the incoming requisition has
   * no modified date, that will not trigger the exception.
   * @param incomingReq the requisition to validate
   * @param existingReq the existing version of the requisition
   * @return ValidationResult that contains outcome of this validation
   */
  public ValidationResult validateRequisitionTimestamps(Requisition incomingReq,
                                                        Requisition existingReq) {
    ZonedDateTime dateModified = incomingReq.getModifiedDate();
    if (dateModified != null
        && !dateModified.isEqual(existingReq.getModifiedDate())) {
      return ValidationResult.conflict(ERROR_DATE_MODIFIED_MISMATCH);
    }
    return ValidationResult.success();
  }
}
