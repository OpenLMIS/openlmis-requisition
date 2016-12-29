package org.openlmis.requisition.validate;

import org.openlmis.requisition.domain.AvailableRequisitionColumnOption;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import java.util.Map;

@Component
public class RequisitionTemplateValidator implements Validator {

  static final String COLUMNS_MAP = "columnsMap";

  static final String REQUESTED_QUANTITY = "requestedQuantity";
  static final String REQUESTED_QUANTITY_EXPLANATION = "requestedQuantityExplanation";
  static final String TOTAL_CONSUMED_QUANTITY = "totalConsumedQuantity";
  static final String ADJUSTED_CONSUMPTION = "adjustedConsumption";
  static final String TOTAL_STOCKOUT_DAYS = "totalStockoutDays";
  static final String STOCK_ON_HAND = "stockOnHand";
  static final String STOCK_ON_HAND_MUST_BE_CALCULATED_INFORMATION =
      " must be displayed when stock on hand is calculated.";
  static final String TOTAL_CONSUMED_QUANTITY_MUST_BE_CALCULATED_INFORMATION =
      " must be displayed when total consumed quantity is calculated.";
  static final String ADJUSTED_CONSUMPTION_MUST_BE_CALCULATED_INFORMATION =
      " must be displayed when adjusted consumption is calculated.";


  @Override
  public boolean supports(Class<?> clazz) {
    return RequisitionTemplate.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    RequisitionTemplate requisitionTemplate = (RequisitionTemplate) target;

    validateRequestedQuantity(errors, requisitionTemplate);
    validateChosenSourcesAndOptions(errors, requisitionTemplate);
    validateCalculatedFields(errors, requisitionTemplate);

    if (!errors.hasErrors()) {
      validateCalculatedField(errors, requisitionTemplate, STOCK_ON_HAND,
          STOCK_ON_HAND_MUST_BE_CALCULATED_INFORMATION, TOTAL_CONSUMED_QUANTITY
      );
      validateCalculatedField(errors, requisitionTemplate, TOTAL_CONSUMED_QUANTITY,
          TOTAL_CONSUMED_QUANTITY_MUST_BE_CALCULATED_INFORMATION, STOCK_ON_HAND
      );
      if (isColumnOnTemplate(requisitionTemplate, ADJUSTED_CONSUMPTION)) {
        validateCalculatedField(errors, requisitionTemplate, ADJUSTED_CONSUMPTION,
            ADJUSTED_CONSUMPTION_MUST_BE_CALCULATED_INFORMATION, TOTAL_CONSUMED_QUANTITY,
            TOTAL_STOCKOUT_DAYS
        );
      }
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

  private void validateChosenSourcesAndOptions(Errors errors, RequisitionTemplate template) {
    for (RequisitionTemplateColumn column : template.getColumnsMap().values()) {
      validateChosenSources(errors, column);
      validateChosenOptions(errors, column);
    }
  }

  private void validateChosenSources(Errors errors, RequisitionTemplateColumn column) {
    SourceType chosenSource = column.getSource();
    if (chosenSource != null
        && !column.getColumnDefinition().getSources().contains(chosenSource)) {
      errors.rejectValue(COLUMNS_MAP, RequisitionTemplate.SOURCE + chosenSource.toString()
          + RequisitionTemplate.WARNING_SUFFIX);
    }
  }

  private void validateChosenOptions(Errors errors, RequisitionTemplateColumn column) {
    AvailableRequisitionColumnOption chosenOption = column.getOption();
    if (chosenOption != null
        && !column.getColumnDefinition().getOptions().contains(chosenOption)) {
      errors.rejectValue(COLUMNS_MAP, RequisitionTemplate.OPTION + chosenOption.getOptionName()
          + RequisitionTemplate.WARNING_SUFFIX);
    }
  }

  private boolean isColumnOnTemplate(RequisitionTemplate requisitionTemplate, String columnName) {
    Map<String, RequisitionTemplateColumn> columnsMap = requisitionTemplate.getColumnsMap();
    RequisitionTemplateColumn adjustedConsumptionColumn = columnsMap.get(columnName);
    return adjustedConsumptionColumn != null;
  }
}
