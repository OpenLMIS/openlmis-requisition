package org.openlmis.requisition.validate;

import static org.springframework.util.CollectionUtils.isEmpty;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
public class RequisitionValidator implements Validator {

  static final String VALUE_MUST_BE_ENTERED_NOTIFICATION =
      " must be entered prior to submission of a requisition.";
  static final String VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION =
      " must be a non-negative value.";
  static final String TEMPLATE_COLUMN_IS_HIDDEN =
      " is hidden in template and should not contain a value.";
  static final String TEMPLATE_COLUMN_IS_CALCULATED =
      " is calculated and should not contain a value";
  static final String IS_ONLY_AVAILABLE_DURING_APPROVAL_STEP =
      " is only available during the approval step of the requisition process.";
  static final String REQUISITION_LINE_ITEMS = "requisitionLineItems";

  static final String REQUESTED_QUANTITY = "requestedQuantity";
  static final String REQUESTED_QUANTITY_EXPLANATION = "requestedQuantityExplanation";
  static final String BEGINNING_BALANCE = "beginningBalance";
  static final String TOTAL_RECEIVED_QUANTITY = "totalReceivedQuantity";
  static final String STOCK_ON_HAND = "stockOnHand";
  static final String TOTAL_CONSUMED_QUANTITY = "totalConsumedQuantity";
  static final String TOTAL_LOSSES_AND_ADJUSTMENTS = "totalLossesAndAdjustments";
  static final String APPROVED_QUANTITY = "approvedQuantity";

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Override
  public boolean supports(Class<?> clazz) {
    return Requisition.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    Requisition requisition = (Requisition) target;

    if (isEmpty(requisition.getRequisitionLineItems())) {
      errors.rejectValue(REQUISITION_LINE_ITEMS,
          "A requisitionLineItems" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    } else {
      RequisitionTemplate template = requisitionTemplateRepository.getTemplateForProgram(
          requisition.getProgramId()
      );

      requisition.getRequisitionLineItems()
          .forEach(i -> validateRequisitionLineItem(errors, template, requisition, i));
    }
  }

  private void validateRequisitionLineItem(Errors errors, RequisitionTemplate template,
                                           Requisition requisition, RequisitionLineItem item) {
    rejectIfNull(errors, template, item.getRequestedQuantity(), REQUESTED_QUANTITY);
    rejectIfLessThanZero(errors, template, item.getRequestedQuantity(), REQUESTED_QUANTITY);

    rejectIfNull(errors, template, item.getBeginningBalance(), BEGINNING_BALANCE);
    rejectIfLessThanZero(errors, template, item.getBeginningBalance(), BEGINNING_BALANCE);

    rejectIfNull(errors, template, item.getTotalReceivedQuantity(), TOTAL_RECEIVED_QUANTITY);
    rejectIfLessThanZero(
        errors, template, item.getTotalReceivedQuantity(), TOTAL_RECEIVED_QUANTITY
    );

    rejectIfNull(errors, template, item.getStockOnHand(), STOCK_ON_HAND);
    rejectIfLessThanZero(errors, template, item.getStockOnHand(), STOCK_ON_HAND);

    rejectIfNull(errors, template, item.getTotalConsumedQuantity(), TOTAL_CONSUMED_QUANTITY);
    rejectIfLessThanZero(
        errors, template, item.getTotalConsumedQuantity(), TOTAL_CONSUMED_QUANTITY
    );

    rejectIfNull(
        errors, template, item.getTotalLossesAndAdjustments(), TOTAL_LOSSES_AND_ADJUSTMENTS
    );

    validateApprovedQuantity(errors, template, requisition, item);

    checkTemplate(errors, template, item.getRequestedQuantityExplanation(),
        REQUESTED_QUANTITY_EXPLANATION);
  }

  private void rejectIfLessThanZero(Errors errors, RequisitionTemplate template,
                                    Integer value, String field) {
    boolean templateValid = checkTemplate(errors, template, value, field);

    if (templateValid && value != null && value < 0) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, field + VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION);
    }
  }

  private void rejectIfNull(Errors errors, RequisitionTemplate template,
                            Object value, String field) {
    boolean templateValid = checkTemplate(errors, template, value, field);

    if (templateValid && value == null) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, field + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    }
  }

  private void validateApprovedQuantity(Errors errors, RequisitionTemplate template,
                                        Requisition requisition, RequisitionLineItem item) {
    Integer value = item.getApprovedQuantity();

    if (requisition.getStatus() == RequisitionStatus.AUTHORIZED) {
      rejectIfNull(errors, template, item.getTotalConsumedQuantity(), TOTAL_CONSUMED_QUANTITY);
      rejectIfLessThanZero(
          errors, template, item.getTotalConsumedQuantity(), TOTAL_CONSUMED_QUANTITY
      );
    } else if (value != null) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, APPROVED_QUANTITY
          + IS_ONLY_AVAILABLE_DURING_APPROVAL_STEP);
    }
  }

  private boolean checkTemplate(Errors errors, RequisitionTemplate template,
                                Object value, String field) {
    try {
      boolean displayed = checkIfDisplayed(errors, template, value, field);
      boolean calculated = checkIfCalculated(errors, template, value, field);

      return displayed && !calculated;
    } catch (RequisitionTemplateColumnException ex) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, ex.getMessage());
    }

    return false;
  }

  private boolean checkIfDisplayed(Errors errors, RequisitionTemplate template, Object value,
                                   String field) throws RequisitionTemplateColumnException {
    if (!template.isColumnDisplayed(field)) {
      if (value != null) {
        errors.rejectValue(REQUISITION_LINE_ITEMS, field + TEMPLATE_COLUMN_IS_HIDDEN);
      }

      return false;
    }

    return true;
  }

  private boolean checkIfCalculated(Errors errors, RequisitionTemplate template, Object value,
                                    String field) throws RequisitionTemplateColumnException {
    if (template.isColumnCalculated(field)) {
      if (value != null) {
        errors.rejectValue(REQUISITION_LINE_ITEMS, field + TEMPLATE_COLUMN_IS_CALCULATED);
      }

      return true;
    }

    return false;
  }
}
