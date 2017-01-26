package org.openlmis.requisition.validate;

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_IS_HIDDEN;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_NON_NEGATIVE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALUE_MUST_BE_ENTERED;

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

  static final String REQUISITION_LINE_ITEMS = "requisitionLineItems";


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
            new Message(ERROR_IS_HIDDEN, field)).toString());
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
          new Message(ERROR_MUST_BE_NON_NEGATIVE, field)).toString());
    }
  }

  protected void rejectIfNull(Errors errors, RequisitionTemplate template,
                              Object value, String field) {
    boolean templateValid = checkTemplate(errors, template, value, field);

    if (templateValid && value == null) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, messageService.localize(
          new Message(ERROR_VALUE_MUST_BE_ENTERED, field)).toString());
    }
  }
}
