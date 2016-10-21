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
  static final String STOCK_ON_HAND_MUST_BE_CALCULATED_INFORMATION =
      " must be displayed when stock on hand is calculated.";
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
    validateCalculatedFields(errors, requisitionTemplate);

    if (!errors.hasErrors()) {
      validateCalculatedField(errors, requisitionTemplate, STOCK_ON_HAND,
          STOCK_ON_HAND_MUST_BE_CALCULATED_INFORMATION, TOTAL_CONSUMED_QUANTITY
      );
      validateCalculatedField(errors, requisitionTemplate, TOTAL_CONSUMED_QUANTITY,
          TOTAL_CONSUMED_QUANTITY_MUST_BE_CALCULATED_INFORMATION,
          BEGINNING_BALANCE, TOTAL_RECEIVED_QUANTITY, TOTAL_LOSSES_AND_ADJUSTMENTS, STOCK_ON_HAND
      );
    }
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

  private void validateCalculatedFields(Errors errors, RequisitionTemplate template) {
    try {
      if (template.isColumnCalculated(TOTAL_CONSUMED_QUANTITY)
          && template.isColumnCalculated(STOCK_ON_HAND)) {
        errors.rejectValue(COLUMNS_MAP, TOTAL_CONSUMED_QUANTITY + " and " + STOCK_ON_HAND
            + "columns cannot be calculated at the same time");
      }
    } catch (RequisitionTemplateColumnException ex) {
      errors.rejectValue(COLUMNS_MAP, ex.getMessage());
    }
  }

  private void validateCalculatedField(Errors errors, RequisitionTemplate template, String field,
                                       String suffix, String... requiredFields) {
    try {
      if (template.isColumnCalculated(field)) {
        for (String requiredField : requiredFields) {
          rejectIfNotDisplayed(errors, template, requiredField, suffix);
        }
      }
    } catch (RequisitionTemplateColumnException ex) {
      errors.rejectValue(COLUMNS_MAP, ex.getMessage());
    }
  }

  private void rejectIfNotDisplayed(Errors errors, RequisitionTemplate template,
                                    String field, String suffix)
      throws RequisitionTemplateColumnException {
    if (!template.isColumnDisplayed(field)) {
      errors.rejectValue(COLUMNS_MAP, field + suffix);
    }
  }
}
