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

import static org.apache.commons.lang3.StringUtils.length;

import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Collection;
import java.util.regex.Pattern;

abstract class BaseValidator implements Validator {
  private static final String ALPHANUMERIC_REGEX = "^[a-zA-z0-9/]+[a-zA-Z0-9/ ]+$";
  private static final Pattern ALPHANUMERIC_PATTERN = Pattern.compile(ALPHANUMERIC_REGEX);
  
  @Autowired
  private MessageService messageService;

  void rejectIfNotDisplayed(Errors errors, RequisitionTemplate template, String column,
                            String field, Message message) {
    if (!template.isColumnDisplayed(column)) {
      rejectValue(errors, field, message);
    }
  }

  void rejectIfDisplayed(Errors errors, RequisitionTemplate template, String column,
                            String field, Message message) {
    if (template.isColumnDisplayed(column)) {
      rejectValue(errors, field, message);
    }
  }

  <T> void rejectIfNotContains(Errors errors, Collection<T> collection, T value, String field,
                               Message message) {
    if (!collection.contains(value)) {
      rejectValue(errors, field, message);
    }
  }

  void rejectIfLengthTooLong(Errors errors, String value, int max, String field, Message message) {
    if (length(value) > max) {
      rejectValue(errors, field, message);
    }
  }

  void rejectIfNotAlphanumeric(Errors errors, String value, String field, Message message) {
    if (null == value || !ALPHANUMERIC_PATTERN.matcher(value).find()) {
      rejectValue(errors, field, message);
    }
  }

  void rejectValue(Errors errors, String field, Message message) {
    errors.rejectValue(field, messageService.localize(message).toString());
  }

}
