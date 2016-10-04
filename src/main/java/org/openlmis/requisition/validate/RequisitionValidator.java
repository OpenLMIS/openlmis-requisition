package org.openlmis.requisition.validate;

import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Map;

import static org.apache.commons.lang.BooleanUtils.isFalse;
import static org.apache.commons.lang.StringUtils.equalsIgnoreCase;
import static org.springframework.util.CollectionUtils.isEmpty;

@Component
public class RequisitionValidator implements Validator {

  private static final String VALUE_MUST_BE_ENTERED_NOTIFICATION =
      " must be entered prior to submission of a requisition.";
  private static final String VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION =
      " must be a non-negative value.";
  private static final String TEMPLATE_NOT_CONTAIN_FIELD =
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
        "A quantity", "J"
    );
    rejectIfLessThanZero(
        errors, template, item.getBeginningBalance(),
        "A beginning balance", null
    );
    rejectIfLessThanZero(
        errors, template, item.getTotalReceivedQuantity(),
        "A total received quantity", null
    );
    rejectIfNull(
        errors, template, item.getStockOnHand(),
        "A total stock on hand", null
    );
    rejectIfNull(
        errors, template, item.getTotalConsumedQuantity(),
        "A total consumed quantity", null
    );
    rejectIfNull(
        errors, template, item.getTotalLossesAndAdjustments(),
        "A total losses and adjustments", null
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
      RequisitionTemplateColumn column = findColumn(template, indicator);

      if (null == column) {
        if (value != null) {
          errors.rejectValue(
              REQUISITION_LINE_ITEMS,
              field + TEMPLATE_NOT_CONTAIN_FIELD
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

  private RequisitionTemplateColumn findColumn(RequisitionTemplate template, String indicator) {
    if (null == template) {
      return null;
    }

    Map<String, RequisitionTemplateColumn> columns = template.getColumnsMap();

    if (null == columns || columns.isEmpty()) {
      return null;
    }

    return columns.values().stream()
        .filter(c -> findColumn(c, indicator))
        .findFirst().orElse(null);
  }

  private boolean findColumn(RequisitionTemplateColumn column, String indicator) {
    if (null == column) {
      return false;
    }

    AvailableRequisitionColumn definition = column.getColumnDefinition();

    if (null == definition) {
      return false;
    }

    return equalsIgnoreCase(definition.getIndicator(), indicator);
  }
}
