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

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_IS_HIDDEN;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_NON_NEGATIVE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALUE_MUST_BE_ENTERED;

import org.apache.commons.lang3.StringUtils;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.utils.Message;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
public abstract class AbstractRequisitionValidator extends BaseValidator {
  static final String REQUISITION_LINE_ITEMS = "requisitionLineItems";

  public boolean supports(Class<?> clazz) {
    return Requisition.class.equals(clazz);
  }

  boolean checkTemplate(Errors errors, RequisitionTemplate template,
                        Object value, String field) {
    return checkIfDisplayed(errors, template, value, field);
  }

  private boolean checkIfDisplayed(Errors errors, RequisitionTemplate template, Object value,
                                   String field) {
    if (!template.isColumnDisplayed(field)) {
      if (value != null) {
        rejectValue(errors, REQUISITION_LINE_ITEMS, new Message(ERROR_IS_HIDDEN, field));
      }

      return false;
    }

    return true;
  }

  void rejectIfNullOrNegative(Errors errors, RequisitionTemplate template,
                              Integer value, String field) {
    rejectIfLessThanZero(errors, template, value, field);
    rejectIfNull(errors, template, value, field);
  }

  void rejectIfLessThanZero(Errors errors, RequisitionTemplate template,
                            Integer value, String field) {
    boolean templateValid = checkTemplate(errors, template, value, field);

    if (templateValid && value != null && value < 0) {
      rejectValue(errors, REQUISITION_LINE_ITEMS, new Message(ERROR_MUST_BE_NON_NEGATIVE, field));
    }
  }

  void rejectIfNull(Errors errors, RequisitionTemplate template, Object value, String field) {
    boolean templateValid = checkTemplate(errors, template, value, field);

    if (templateValid && value == null) {
      rejectValue(errors, REQUISITION_LINE_ITEMS, new Message(ERROR_VALUE_MUST_BE_ENTERED, field));
    }
  }

  void rejectIfEmpty(Errors errors, RequisitionTemplate template, String value, String field) {
    boolean templateValid = checkTemplate(errors, template, value, field);

    if (templateValid && StringUtils.isBlank(value)) {
      rejectValue(errors, REQUISITION_LINE_ITEMS, new Message(ERROR_VALUE_MUST_BE_ENTERED, field));
    }
  }
}
