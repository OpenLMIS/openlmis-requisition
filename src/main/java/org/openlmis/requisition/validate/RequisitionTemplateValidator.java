package org.openlmis.requisition.validate;

import static org.apache.commons.lang3.StringUtils.length;
import static org.openlmis.requisition.domain.RequisitionTemplateColumn.DEFINITION;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_CANNOT_CALCULATE_AT_THE_SAME_TIME;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DISPLAYED_WHEN_REQUESTED_QUANTITY_EXPLANATION_IS_DISPLAYED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DISPLAYED_WHEN_REQUESTED_QUANTITY_IS_DISPLAYED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_DISPLAYED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMED_QUANTITY_IS_CALCULATED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_DISPLAYED_WHEN_ON_HAND_IS_CALCULATED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_OPTION_NOT_AVAILABLE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SOURCE_NOT_AVAILABLE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_COLUMN_DEFINITION_MODIFIED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_COLUMN_DEFINITION_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_FIELD_IS_TOO_LONG;

import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.AvailableRequisitionColumnOption;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.repository.AvailableRequisitionColumnRepository;
import org.openlmis.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Set;

@Component
@SuppressWarnings("PMD.TooManyMethods")
public class RequisitionTemplateValidator implements Validator {
  @Autowired
  MessageService messageService;

  @Autowired
  ValidatorUtil validatorUtil;

  @Autowired
  private AvailableRequisitionColumnRepository availableRequisitionColumnRepository;

  static final String COLUMNS_MAP = "columnsMap";
  static final String NUMBER_OF_PERIODS_TO_AVERAGE = "numberOfPeriodsToAverage";

  static final String REQUESTED_QUANTITY = "requestedQuantity";
  static final String REQUESTED_QUANTITY_EXPLANATION = "requestedQuantityExplanation";
  static final String TOTAL_CONSUMED_QUANTITY = "totalConsumedQuantity";
  static final String ADJUSTED_CONSUMPTION = "adjustedConsumption";
  static final String AVERAGE_CONSUMPTION = "averageConsumption";
  static final String TOTAL_STOCKOUT_DAYS = "totalStockoutDays";
  static final String STOCK_ON_HAND = "stockOnHand";

  static final int MAX_COLUMN_DEFINITION_LENGTH = 140;


  @Override
  public boolean supports(Class<?> clazz) {
    return RequisitionTemplate.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    RequisitionTemplate requisitionTemplate = (RequisitionTemplate) target;

    validateRequestedQuantity(errors, requisitionTemplate);
    validateColumns(errors, requisitionTemplate);
    validateCalculatedFields(errors, requisitionTemplate);

    if (!errors.hasErrors()) {
      validateCalculatedField(errors, requisitionTemplate, STOCK_ON_HAND,
          ERROR_MUST_BE_DISPLAYED_WHEN_ON_HAND_IS_CALCULATED, TOTAL_CONSUMED_QUANTITY
      );
      validateCalculatedField(errors, requisitionTemplate, TOTAL_CONSUMED_QUANTITY,
          ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMED_QUANTITY_IS_CALCULATED, STOCK_ON_HAND
      );
      if (requisitionTemplate.isColumnInTemplate(ADJUSTED_CONSUMPTION)) {
        validateCalculatedField(errors, requisitionTemplate, ADJUSTED_CONSUMPTION,
            ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED, TOTAL_CONSUMED_QUANTITY,
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
            new Message(ERROR_DISPLAYED_WHEN_REQUESTED_QUANTITY_IS_DISPLAYED,
                REQUESTED_QUANTITY_EXPLANATION)).toString());
      }
    } else {
      if (explanationDisplayed) {
        errors.rejectValue(COLUMNS_MAP, messageService.localize(new Message(
            ERROR_DISPLAYED_WHEN_REQUESTED_QUANTITY_EXPLANATION_IS_DISPLAYED,
            REQUESTED_QUANTITY)).toString());

      }
    }
  }

  private void validateCalculatedFields(Errors errors, RequisitionTemplate template) {
    if (template.isColumnCalculated(TOTAL_CONSUMED_QUANTITY)
        && template.isColumnCalculated(STOCK_ON_HAND)) {
      errors.rejectValue(COLUMNS_MAP, messageService.localize(new Message(
          ERROR_CANNOT_CALCULATE_AT_THE_SAME_TIME, TOTAL_CONSUMED_QUANTITY,
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

  private void validateColumns(Errors errors, RequisitionTemplate template) {
    for (RequisitionTemplateColumn column : template.getColumnsMap().values()) {
      validateColumnDefinition(errors, column);
      validateChosenSources(errors, template, column);
      validateChosenOptions(errors, column);
      validateDefinition(errors, column);
    }
  }

  private void validateColumnDefinition(Errors errors, RequisitionTemplateColumn column) {
    AvailableRequisitionColumn actual = column.getColumnDefinition();
    AvailableRequisitionColumn expected = availableRequisitionColumnRepository
        .findOne(actual.getId());

    if (null == expected) {
      errors.rejectValue(COLUMNS_MAP, messageService.localize(
          new Message(ERROR_VALIDATION_COLUMN_DEFINITION_NOT_FOUND, column.getLabel())).toString());
    } else if (!expected.equals(actual)) {
      errors.rejectValue(COLUMNS_MAP, messageService.localize(
          new Message(ERROR_VALIDATION_COLUMN_DEFINITION_MODIFIED, column.getLabel())).toString());
    }
  }

  private void validateChosenSources(Errors errors, RequisitionTemplate template,
                                     RequisitionTemplateColumn column) {
    SourceType chosenSource = column.getSource();

    if (null != chosenSource) {
      AvailableRequisitionColumn definition = column.getColumnDefinition();
      Set<SourceType> sources = definition.getSources();

      if (sources.size() > 1 && template.isColumnUserInput(definition.getName())) {
        rejectIfNotDisplayed(errors, template, definition.getName(), ERROR_MUST_BE_DISPLAYED);
      }

      if (!sources.contains(chosenSource)) {
        errors.rejectValue(COLUMNS_MAP, messageService.localize(
            new Message(ERROR_SOURCE_NOT_AVAILABLE, chosenSource.toString())).toString());
      }
    }
  }

  private void validateChosenOptions(Errors errors, RequisitionTemplateColumn column) {
    AvailableRequisitionColumnOption chosenOption = column.getOption();
    if (chosenOption != null
        && !column.getColumnDefinition().getOptions().contains(chosenOption)) {
      errors.rejectValue(COLUMNS_MAP, messageService.localize(
          new Message(ERROR_OPTION_NOT_AVAILABLE, chosenOption.toString())).toString());
    }
  }

  private void validateDefinition(Errors errors, RequisitionTemplateColumn column) {
    if (length(column.getDefinition()) > MAX_COLUMN_DEFINITION_LENGTH) {
      errors.rejectValue(COLUMNS_MAP, messageService.localize(
          new Message(ERROR_VALIDATION_FIELD_IS_TOO_LONG,
              DEFINITION, MAX_COLUMN_DEFINITION_LENGTH)).toString());
    }
  }
}
