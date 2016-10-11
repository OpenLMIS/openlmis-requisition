package org.openlmis.requisition.validate;

import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
public class RequisitionTemplateValidator implements Validator {

  static final String COLUMNS_MAP = "columnsMap";

  static final String REQUESTED_QUANTITY = "requestedQuantity";
  static final String REQUESTED_QUANTITY_EXPLANATION = "requestedQuantityExplanation";
  static final String TOTAL_CONSUMED_QUANTITY = "totalConsumedQuantity";
  static final String BEGINNING_BALANCE = "beginningBalance";
  static final String TOTAL_RECEIVED_QUANTITY = "totalReceivedQuantity";
  static final String TOTAL_LOSSES_AND_ADJUSTMENTS = "totalLossesAndAdjustments";
  static final String STOCK_ON_HAND = "stockOnHand";
  static final String TOTAL_CONSUMED_QUANTITY_MUST_BE_CALCULATED_INFORMATION =
      " must be displayed when total consumed quantity is calculated.";


  @Override
  public boolean supports(Class<?> clazz) {
    return RequisitionTemplate.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    RequisitionTemplate requisitionTemplate = (RequisitionTemplate) target;

    validateRequestedQuantity(errors, requisitionTemplate);
    validateTotalConsumedQuantity(errors, requisitionTemplate);
  }

  private void validateRequestedQuantity(Errors errors, RequisitionTemplate template) {
    try {
      boolean quantityDisplayed = template.isColumnDisplayed(REQUESTED_QUANTITY);
      boolean explanationDisplayed = template.isColumnDisplayed(REQUESTED_QUANTITY_EXPLANATION);

      if (quantityDisplayed) {
        if (!explanationDisplayed) {
          errors.rejectValue(COLUMNS_MAP, REQUESTED_QUANTITY_EXPLANATION
              + " must be displayed when requested quantity is displayed.");
        }
      } else {
        if (explanationDisplayed) {
          errors.rejectValue(COLUMNS_MAP, REQUESTED_QUANTITY
              + " must be displayed when requested quantity explanation is displayed.");
        }
      }
    } catch (RequisitionTemplateColumnException ex) {
      errors.rejectValue(COLUMNS_MAP, ex.getMessage());
    }
  }

  private void validateTotalConsumedQuantity(Errors errors, RequisitionTemplate template) {
    try {
      boolean consumedQuantityCalculated = template.isColumnCalculated(TOTAL_CONSUMED_QUANTITY);

      if (consumedQuantityCalculated) {
        if (template.isColumnCalculated(STOCK_ON_HAND)) {
          errors.rejectValue(COLUMNS_MAP, TOTAL_CONSUMED_QUANTITY + " and " + STOCK_ON_HAND
              + "columns cannot be calculated at the same time");
        } else {
          rejectIfNotDisplayed(errors, template, BEGINNING_BALANCE);
          rejectIfNotDisplayed(errors, template, TOTAL_RECEIVED_QUANTITY);
          rejectIfNotDisplayed(errors, template, TOTAL_LOSSES_AND_ADJUSTMENTS);
          rejectIfNotDisplayed(errors, template, STOCK_ON_HAND);
        }
      }
    } catch (RequisitionTemplateColumnException ex) {
      errors.rejectValue(COLUMNS_MAP, ex.getMessage());
    }
  }

  private void rejectIfNotDisplayed(Errors errors, RequisitionTemplate template, String field)
      throws RequisitionTemplateColumnException {
    if (!template.isColumnDisplayed(field)) {
      errors.rejectValue(
          COLUMNS_MAP, field + TOTAL_CONSUMED_QUANTITY_MUST_BE_CALCULATED_INFORMATION
      );
    }
  }
}
