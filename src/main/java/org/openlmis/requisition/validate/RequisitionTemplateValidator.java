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

  @Override
  public boolean supports(Class<?> clazz) {
    return RequisitionTemplate.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    RequisitionTemplate requisitionTemplate = (RequisitionTemplate) target;

    validateRequestedQuantity(errors, requisitionTemplate);
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
}
