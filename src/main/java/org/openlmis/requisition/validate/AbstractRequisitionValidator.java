package org.openlmis.requisition.validate;

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_IS_HIDDEN;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_NON_NEGATIVE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALUE_MUST_BE_ENTERED;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.utils.Message;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
abstract class AbstractRequisitionValidator extends BaseValidator {
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

  void rejectIfNull(Errors errors, RequisitionTemplate template,
                    Object value, String field) {
    boolean templateValid = checkTemplate(errors, template, value, field);

    if (templateValid && value == null) {
      rejectValue(errors, REQUISITION_LINE_ITEMS, new Message(ERROR_VALUE_MUST_BE_ENTERED, field));
    }
  }
}
