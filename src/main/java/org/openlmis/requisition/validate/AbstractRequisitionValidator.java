package org.openlmis.requisition.validate;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
abstract class AbstractRequisitionValidator implements Validator {

  @Autowired
  MessageService messageService;

  static final String TEMPLATE_COLUMN_IS_CALCULATED =
      "requisition.error.validation.field-is-calculated";
  static final String IS_ONLY_AVAILABLE_DURING_APPROVAL_STEP =
      "requisition.error.validation.only-available-for-approval";
  static final String REQUISITION_LINE_ITEMS = "requisitionLineItems";
  static final String VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION =
      "requisition.error.validation.must-be-non-negative";
  static final String VALUE_MUST_BE_ENTERED_NOTIFICATION =
      "requisition.error.validation.value-must-be-entered";
  static final String TEMPLATE_COLUMN_IS_HIDDEN =
      "requisition.error.validation.is-hidden";
  static final String IS_INVARIANT =
      "requisition.error.validation.is-invariant";

  public boolean supports(Class<?> clazz) {
    return Requisition.class.equals(clazz);
  }

  protected boolean checkTemplate(Errors errors, RequisitionTemplate template,
                                  Object value, String field) {
    return checkIfDisplayed(errors, template, value, field);
  }

  protected boolean checkIfDisplayed(Errors errors, RequisitionTemplate template, Object value,
                                     String field) {
    if (!template.isColumnDisplayed(field)) {
      if (value != null) {
        errors.rejectValue(REQUISITION_LINE_ITEMS, messageService.localize(
            new Message(TEMPLATE_COLUMN_IS_HIDDEN, field)).toString());
      }

      return false;
    }

    return true;
  }

  protected void rejectIfNullOrNegative(Errors errors, RequisitionTemplate template,
                                        Integer value, String field) {
    rejectIfLessThanZero(errors, template, value, field);
    rejectIfNull(errors, template, value, field);
  }

  protected void rejectIfLessThanZero(Errors errors, RequisitionTemplate template,
                                      Integer value, String field) {
    boolean templateValid = checkTemplate(errors, template, value, field);

    if (templateValid && value != null && value < 0) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, messageService.localize(
          new Message(VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION, field)).toString());
    }
  }

  protected void rejectIfNull(Errors errors, RequisitionTemplate template,
                              Object value, String field) {
    boolean templateValid = checkTemplate(errors, template, value, field);

    if (templateValid && value == null) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, messageService.localize(
          new Message(VALUE_MUST_BE_ENTERED_NOTIFICATION, field)).toString());
    }
  }
}
