package org.openlmis.requisition.validate;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import static org.openlmis.requisition.validate.RequisitionValidator.TEMPLATE_COLUMN_IS_HIDDEN;
import static org.openlmis.requisition.validate.RequisitionValidator.VALUE_MUST_BE_ENTERED_NOTIFICATION;
import static org.openlmis.requisition.validate.RequisitionValidator.VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION;

abstract class AbstractRequisitionValidator implements Validator {

  static final String TEMPLATE_COLUMN_IS_CALCULATED =
      " is calculated and should not contain a value";
  static final String IS_ONLY_AVAILABLE_DURING_APPROVAL_STEP =
      " is only available during the approval step of the requisition process.";
  static final String REQUISITION_LINE_ITEMS = "requisitionLineItems";
  static final String VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION =
      " must be a non-negative value.";
  static final String VALUE_MUST_BE_ENTERED_NOTIFICATION =
      " must be entered prior to submission of a requisition.";
  static final String TEMPLATE_COLUMN_IS_HIDDEN =
      " is hidden in template and should not contain a value.";

  public boolean supports(Class<?> clazz) {
    return Requisition.class.equals(clazz);
  }

  protected boolean checkTemplate(Errors errors, RequisitionTemplate template,
                                Object value, String field) {
    try {
      return checkIfDisplayed(errors, template, value, field);
    } catch (RequisitionTemplateColumnException ex) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, ex.getMessage());
    }

    return false;
  }

  protected boolean checkIfDisplayed(Errors errors, RequisitionTemplate template, Object value,
                                   String field) throws RequisitionTemplateColumnException {
    if (!template.isColumnDisplayed(field)) {
      if (value != null) {
        errors.rejectValue(REQUISITION_LINE_ITEMS, field + TEMPLATE_COLUMN_IS_HIDDEN);
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
      errors.rejectValue(REQUISITION_LINE_ITEMS, field + VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION);
    }
  }

  protected void rejectIfNull(Errors errors, RequisitionTemplate template,
                            Object value, String field) {
    boolean templateValid = checkTemplate(errors, template, value, field);

    if (templateValid && value == null) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, field + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    }
  }
}
