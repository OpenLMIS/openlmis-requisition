package org.openlmis.requisition.validate;

import static org.apache.commons.lang3.StringUtils.length;

import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Collection;

abstract class BaseValidator implements Validator {

  @Autowired
  private MessageService messageService;

  void rejectIfNotDisplayed(Errors errors, RequisitionTemplate template, String column,
                            String field, Message message) {
    if (!template.isColumnDisplayed(column)) {
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

  void rejectValue(Errors errors, String field, Message message) {
    errors.rejectValue(field, messageService.localize(message).toString());
  }

}
