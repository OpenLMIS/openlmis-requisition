package org.openlmis.requisition.validate;

import org.openlmis.requisition.domain.AvailableRequisitionColumnOption;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
public class RequisitionTemplateValidator implements Validator {

  @Autowired
  MessageService messageService;

  @Autowired
  ValidatorUtil validatorUtil;

  static final String COLUMNS_MAP = "columnsMap";
  static final String NUMBER_OF_PERIODS_TO_AVERAGE = "numberOfPeriodsToAverage";

  static final String REQUESTED_QUANTITY = "requestedQuantity";
  static final String REQUESTED_QUANTITY_EXPLANATION = "requestedQuantityExplanation";
  static final String TOTAL_CONSUMED_QUANTITY = "totalConsumedQuantity";
  static final String ADJUSTED_CONSUMPTION = "adjustedConsumption";
  static final String AVERAGE_CONSUMPTION = "averageConsumption";
  static final String TOTAL_STOCKOUT_DAYS = "totalStockoutDays";
  static final String STOCK_ON_HAND = "stockOnHand";
  static final String STOCK_ON_HAND_MUST_BE_CALCULATED_INFORMATION =
      "requisition.error.validation.must-be-displayed-when-on-hand-calculated";
  static final String TOTAL_CONSUMED_QUANTITY_MUST_BE_CALCULATED_INFORMATION =
      "requisition.error.validation.mist-be-displayed-when-consumed-quantity-is-calculated";
  static final String ADJUSTED_CONSUMPTION_MUST_BE_CALCULATED_INFORMATION =
      "requisition.error.validation.must-be-displayed-when-consumption-is-calculated";


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
      if (requisitionTemplate.isColumnInTemplate(ADJUSTED_CONSUMPTION)) {
        validateCalculatedField(errors, requisitionTemplate, ADJUSTED_CONSUMPTION,
            ADJUSTED_CONSUMPTION_MUST_BE_CALCULATED_INFORMATION, TOTAL_CONSUMED_QUANTITY,
            TOTAL_STOCKOUT_DAYS
        );
      }
      if (requisitionTemplate.isColumnInTemplate(AVERAGE_CONSUMPTION)) {
        if (!requisitionTemplate.isColumnInTemplate(ADJUSTED_CONSUMPTION)) {
          validatorUtil.rejectValue(errors, COLUMNS_MAP, messageService.localize(
              new Message("requisition.error.validation.fieldMustBeInTemplate",
                  AVERAGE_CONSUMPTION, ADJUSTED_CONSUMPTION)));
        }
        if (requisitionTemplate.getNumberOfPeriodsToAverage() == null) {
          validatorUtil.rejectValue(errors, NUMBER_OF_PERIODS_TO_AVERAGE, messageService.localize(
              new Message("requisition.error.validation.fieldCanNotBeNull",
                  NUMBER_OF_PERIODS_TO_AVERAGE, AVERAGE_CONSUMPTION)));
        }
      }
    }

    if (requisitionTemplate.getNumberOfPeriodsToAverage() != null
        && requisitionTemplate.getNumberOfPeriodsToAverage() < 2) {
      validatorUtil.rejectValue(errors, NUMBER_OF_PERIODS_TO_AVERAGE, messageService.localize(
          new Message("requisition.error.validation.fieldMustBeGreaterOrEqual",
              NUMBER_OF_PERIODS_TO_AVERAGE, "2")));
    }
  }

  private void validateRequestedQuantity(Errors errors, RequisitionTemplate template) {
    boolean quantityDisplayed = template.isColumnDisplayed(REQUESTED_QUANTITY);
    boolean explanationDisplayed = template.isColumnDisplayed(REQUESTED_QUANTITY_EXPLANATION);

    if (quantityDisplayed) {
      if (!explanationDisplayed) {
        errors.rejectValue(COLUMNS_MAP, messageService.localize(
            new Message("requisition.error.validation.displayed-when-requested-quantity-displayed",
                REQUESTED_QUANTITY_EXPLANATION)).toString());
      }
    } else {
      if (explanationDisplayed) {
        errors.rejectValue(COLUMNS_MAP, messageService.localize(new Message(
            "requisition.error.validation.displayed-when-requested-quantity-explanation-displayed",
            REQUESTED_QUANTITY)).toString());

      }
    }
  }

  private void validateCalculatedFields(Errors errors, RequisitionTemplate template) {
    if (template.isColumnCalculated(TOTAL_CONSUMED_QUANTITY)
        && template.isColumnCalculated(STOCK_ON_HAND)) {
      errors.rejectValue(COLUMNS_MAP, messageService.localize(new Message(
          "requisition.error.validation.cannot-calculate-at-the-same-time", TOTAL_CONSUMED_QUANTITY,
          STOCK_ON_HAND)).toString());
    }
  }

  private void validateCalculatedField(Errors errors, RequisitionTemplate template, String field,
                                       String suffix, String... requiredFields) {
    if (template.isColumnCalculated(field)) {
      for (String requiredField : requiredFields) {
        if (!template.isColumnInTemplate(requiredField)) {
          errors.rejectValue(COLUMNS_MAP, messageService.localize(new Message(
              "requisition.error.validation.fieldMustBeInTemplate", requiredField, field))
              .toString());
        }
        if (template.isColumnUserInput(requiredField)) {
          rejectIfNotDisplayed(errors, template, requiredField, suffix);
        }
      }
    }
  }

  private void rejectIfNotDisplayed(Errors errors, RequisitionTemplate template,
                                    String field, String suffix) {
    if (!template.isColumnDisplayed(field)) {
      errors.rejectValue(COLUMNS_MAP, messageService.localize(
          new Message(suffix, field)).toString());
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
      errors.rejectValue(COLUMNS_MAP, messageService.localize(
          new Message("requisition.error.validation.source-is-not-available",
              chosenSource.toString())).toString());
    }
  }

  private void validateChosenOptions(Errors errors, RequisitionTemplateColumn column) {
    AvailableRequisitionColumnOption chosenOption = column.getOption();
    if (chosenOption != null
        && !column.getColumnDefinition().getOptions().contains(chosenOption)) {
      errors.rejectValue(COLUMNS_MAP, messageService.localize(
          new Message("requisition.error.validation.option-is-not-available",
              chosenOption.toString())).toString());
    }
  }
}
