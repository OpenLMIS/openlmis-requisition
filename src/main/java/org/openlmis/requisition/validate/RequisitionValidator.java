package org.openlmis.requisition.validate;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import static org.apache.commons.lang.BooleanUtils.isFalse;
import static org.springframework.util.CollectionUtils.isEmpty;

@Component
public class RequisitionValidator implements Validator {

  private static final String VALUE_MUST_BE_ENTERED_NOTIFICATION =
      " must be entered prior to submission of a requisition.";
  private static final String VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION =
      " must be a non-negative value.";
  private static final String TEMPLATE_DOESNT_CONTAIN_FIELD =
      " is not present in template";
  private static final String TEMPLATE_COLUMN_IS_HIDDEN =
      " is hidden in template and should not contain a value";
  private static final String REQUISITION_LINE_ITEMS = "requisitionLineItems";

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
          requisition.getProgram()
      );

      requisition.getRequisitionLineItems()
          .forEach(i -> validateRequisitionLineItem(errors, template, i));
    }
  }

  private void validateRequisitionLineItem(Errors errors, RequisitionTemplate template,
                                           RequisitionLineItem item) {
    rejectIfLessThanZero(
        errors, template, item.getRequestedQuantity(),
        "Quantity", "J"
    );
    rejectIfLessThanZero(
        errors, template, item.getBeginningBalance(),
        "Beginning balance", null
    );
    rejectIfLessThanZero(
        errors, template, item.getTotalReceivedQuantity(),
        "Total received quantity", "B"
    );
    rejectIfNull(
        errors, template, item.getStockOnHand(),
        "Total stock on hand", "E"
    );
    rejectIfNull(
        errors, template, item.getTotalConsumedQuantity(),
        "Total consumed quantity", null
    );
    rejectIfNull(
        errors, template, item.getTotalLossesAndAdjustments(),
        "Total losses and adjustments", null
    );
  }

  private void rejectIfLessThanZero(Errors errors, RequisitionTemplate template,
                                    Integer value, String field, String indicator) {
    boolean rejected = rejectIfNull(errors, template, value, field, indicator);

    if (!rejected && value < 0) {
      errors.rejectValue(
          REQUISITION_LINE_ITEMS,
          field + VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION);
    }
  }

  private boolean rejectIfNull(Errors errors, RequisitionTemplate template,
                               Object value, String field, String indicator) {
    boolean ok = checkTemplate(errors, template, value, field, indicator);

    if (ok && value == null) {
      errors.rejectValue(
          REQUISITION_LINE_ITEMS,
          field + VALUE_MUST_BE_ENTERED_NOTIFICATION);
      return true;
    }

    return !ok;
  }

  private boolean checkTemplate(Errors errors, RequisitionTemplate template,
                                Object value, String field, String indicator) {
    if (null != indicator) {
      RequisitionTemplateColumn column = null == template ? null : template.findColumn(indicator);

      if (null == column) {
        if (value != null) {
          errors.rejectValue(
              REQUISITION_LINE_ITEMS,
              field + TEMPLATE_DOESNT_CONTAIN_FIELD
          );
          return false;
        }
      } else {
        if (isFalse(column.getIsDisplayed()) && value != null) {
          errors.rejectValue(
              REQUISITION_LINE_ITEMS,
              field + TEMPLATE_COLUMN_IS_HIDDEN
          );
          return false;
        }
      }
    }

    return true;
  }

}
