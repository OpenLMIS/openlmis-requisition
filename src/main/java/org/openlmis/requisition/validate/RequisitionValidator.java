package org.openlmis.requisition.validate;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;

public class RequisitionValidator implements Validator {

  @Override
  public boolean supports(Class<?> clazz) {
    return Requisition.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    ValidationUtils.rejectIfEmpty(errors, "requisitionLines", "RequisitionLines list is empty");

    Requisition requisition = (Requisition) target;

    if (requisition.getRequisitionLines() != null) {
      for (RequisitionLine requisitionLine : requisition.getRequisitionLines()) {
        if (requisitionLine.getRequestedQuantity() == null) {
          errors.rejectValue("requisitionLines",
              "A quantity must be entered prior to submission of a requisition.");
        }
      }
    }

  }
}