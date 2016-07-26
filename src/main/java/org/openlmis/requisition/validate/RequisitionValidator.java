package org.openlmis.requisition.validate;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

public class RequisitionValidator implements Validator {

  private static String VALUE_MUST_BE_ENTERED_NOTIFICATION =
          " must be entered prior to submission of a requisition.";
  private static String REQUISITION_LINES = "requisitionLines";

  @Override
  public boolean supports(Class<?> clazz) {
    return Requisition.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {

    Requisition requisition = (Requisition) target;

    if (requisition.getRequisitionLines() == null || requisition.getRequisitionLines().isEmpty()) {
      errors.rejectValue(REQUISITION_LINES,
          "A requisitionLines" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
      return;
    }

    for (RequisitionLine requisitionLine : requisition.getRequisitionLines()) {
      validateRequisitionLine(errors,requisitionLine);
    }
  }

  private void validateRequisitionLine(Errors errors, RequisitionLine requisitionLine) {
    if (requisitionLine.getRequestedQuantity() == null) {
      errors.rejectValue(
              REQUISITION_LINES,
              "A quantity" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    }
    if (requisitionLine.getBeginningBalance() == null) {
      errors.rejectValue(
              REQUISITION_LINES,
              "A beginning balance" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    }
    if (requisitionLine.getTotalReceivedQuantity() == null) {
      errors.rejectValue(
              REQUISITION_LINES,
              "A total received quantity" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    }
    if (requisitionLine.getStockOnHand() == null) {
      errors.rejectValue(
              REQUISITION_LINES,
              "A total stock on hand" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    }
    if (requisitionLine.getTotalConsumedQuantity() == null) {
      errors.rejectValue(
              REQUISITION_LINES,
              "A total consumed quantity" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    }
    if (requisitionLine.getTotalLossesAndAdjustments() == null) {
      errors.rejectValue(
              REQUISITION_LINES,
              "A total losses and adjustments" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    }
  }
}