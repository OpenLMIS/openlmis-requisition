package org.openlmis.requisition.validate;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

public class RequisitionValidator implements Validator {

  private static final  String VALUE_MUST_BE_ENTERED_NOTIFICATION =
          " must be entered prior to submission of a requisition.";
  private static final  String VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION =
          " must be a non-negative value.";
  private static final  String REQUISITION_LINE_ITEMS = "requisitionLineItems";

  @Override
  public boolean supports(Class<?> clazz) {
    return Requisition.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {

    Requisition requisition = (Requisition) target;

    if (requisition.getRequisitionLineItems() == null
        || requisition.getRequisitionLineItems().isEmpty()) {
      errors.rejectValue(REQUISITION_LINE_ITEMS,
          "A requisitionLineItems" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
      return;
    }

    for (RequisitionLineItem requisitionLineItem : requisition.getRequisitionLineItems()) {
      validateRequisitionLineItem(errors, requisitionLineItem);
    }
  }

  private void validateRequisitionLineItem(Errors errors, RequisitionLineItem requisitionLineItem) {
    if (requisitionLineItem.getRequestedQuantity() == null) {
      errors.rejectValue(
          REQUISITION_LINE_ITEMS,
              "A quantity" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    }
    if (requisitionLineItem.getBeginningBalance() == null) {
      errors.rejectValue(
          REQUISITION_LINE_ITEMS,
              "A beginning balance" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    } else if (requisitionLineItem.getBeginningBalance() < 0) {
      errors.rejectValue(
          REQUISITION_LINE_ITEMS,
              "A beginning balance" + VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION);
    }
    if (requisitionLineItem.getTotalReceivedQuantity() == null) {
      errors.rejectValue(
          REQUISITION_LINE_ITEMS,
              "A total received quantity" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    } else if (requisitionLineItem.getTotalReceivedQuantity() < 0) {
      errors.rejectValue(
          REQUISITION_LINE_ITEMS,
              "A total received quantity" + VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION);
    }
    if (requisitionLineItem.getStockOnHand() == null) {
      errors.rejectValue(
          REQUISITION_LINE_ITEMS,
              "A total stock on hand" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    }
    if (requisitionLineItem.getTotalConsumedQuantity() == null) {
      errors.rejectValue(
          REQUISITION_LINE_ITEMS,
              "A total consumed quantity" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    }
    if (requisitionLineItem.getTotalLossesAndAdjustments() == null) {
      errors.rejectValue(
          REQUISITION_LINE_ITEMS,
              "A total losses and adjustments" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    }
  }
}