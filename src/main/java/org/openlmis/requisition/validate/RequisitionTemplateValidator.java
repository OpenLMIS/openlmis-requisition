/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

package org.openlmis.requisition.validate;

import static org.openlmis.requisition.domain.RequisitionTemplateColumn.DEFINITION;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_CANNOT_CALCULATE_AT_THE_SAME_TIME;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DISPLAYED_WHEN_REQUESTED_QUANTITY_EXPLANATION_IS_DISPLAYED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DISPLAYED_WHEN_REQUESTED_QUANTITY_IS_DISPLAYED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_DISPLAYED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMED_QUANTITY_IS_CALCULATED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_DISPLAYED_WHEN_ON_HAND_IS_CALCULATED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_OPTION_NOT_AVAILABLE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SOURCE_NOT_AVAILABLE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SOURCE_OF_REQUISITION_TEMPLATE_COLUMN_CANNOT_BE_NULL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_COLUMN_DEFINITION_MODIFIED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_COLUMN_DEFINITION_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_FIELD_IS_TOO_LONG;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_FIELD_MUST_BE_IN_TEMPLATE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_REFERENCED_OBJECT_DOES_NOT_EXIST;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DISPLAYED_WHEN_CALC_ORDER_QUANTITY_EXPLANATION_NOT_DISPLAYED;

import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.AvailableRequisitionColumnOption;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.repository.AvailableRequisitionColumnRepository;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

import java.util.Optional;
import java.util.Set;
import java.util.UUID;

@Component
public class RequisitionTemplateValidator extends BaseValidator {

  static final String COLUMNS_MAP = "columnsMap";
  static final String NUMBER_OF_PERIODS_TO_AVERAGE = "numberOfPeriodsToAverage";
  static final String PROGRAM_ID = "programId";
  static final String PROGRAM = "program";
  static final String REQUESTED_QUANTITY = "requestedQuantity";
  static final String REQUESTED_QUANTITY_EXPLANATION = "requestedQuantityExplanation";
  static final String TOTAL_CONSUMED_QUANTITY = "totalConsumedQuantity";
  static final String ADJUSTED_CONSUMPTION = "adjustedConsumption";
  static final String AVERAGE_CONSUMPTION = "averageConsumption";
  static final String TOTAL_STOCKOUT_DAYS = "totalStockoutDays";
  static final String STOCK_ON_HAND = "stockOnHand";
  static final String CALCULATED_ORDER_QUANTITY = "calculatedOrderQuantity";
  static final int MAX_COLUMN_DEFINITION_LENGTH = 140;

  @Autowired
  private AvailableRequisitionColumnRepository availableRequisitionColumnRepository;
  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

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
          rejectValue(errors, COLUMNS_MAP,
              new Message("requisition.error.validation.fieldMustBeInTemplate",
                  AVERAGE_CONSUMPTION, ADJUSTED_CONSUMPTION));
        }
        if (requisitionTemplate.getNumberOfPeriodsToAverage() == null) {
          rejectValue(errors, NUMBER_OF_PERIODS_TO_AVERAGE,
              new Message("requisition.error.validation.fieldCanNotBeNull",
                  NUMBER_OF_PERIODS_TO_AVERAGE, AVERAGE_CONSUMPTION));
        }
      }
    }

    if (requisitionTemplate.getNumberOfPeriodsToAverage() != null
        && requisitionTemplate.getNumberOfPeriodsToAverage() < 2) {
      rejectValue(errors, NUMBER_OF_PERIODS_TO_AVERAGE,
          new Message("requisition.error.validation.fieldMustBeGreaterOrEqual",
              NUMBER_OF_PERIODS_TO_AVERAGE, "2"));
    }

    UUID programId = requisitionTemplate.getProgramId();
    if (null != programId && null == programReferenceDataService.findOne(programId)) {
      rejectValue(errors, PROGRAM_ID,
          new Message(ERROR_VALIDATION_REFERENCED_OBJECT_DOES_NOT_EXIST,
              PROGRAM, programId));
    }
  }

  private void validateRequestedQuantity(Errors errors, RequisitionTemplate template) {
    boolean quantityDisplayed = template.isColumnDisplayed(REQUESTED_QUANTITY);
    boolean explanationDisplayed = template.isColumnDisplayed(REQUESTED_QUANTITY_EXPLANATION);
    boolean calcOrderQuantityDisplayed =
        template.isColumnDisplayed(CALCULATED_ORDER_QUANTITY);

    if (quantityDisplayed) {
      if (!explanationDisplayed) {
        rejectValue(errors, COLUMNS_MAP,
            new Message(ERROR_DISPLAYED_WHEN_REQUESTED_QUANTITY_IS_DISPLAYED,
                REQUESTED_QUANTITY_EXPLANATION));
      }
    } else {
      if (explanationDisplayed) {
        rejectValue(errors, COLUMNS_MAP,
            new Message(ERROR_DISPLAYED_WHEN_REQUESTED_QUANTITY_EXPLANATION_IS_DISPLAYED,
                REQUESTED_QUANTITY));
      }
    }
    if (!calcOrderQuantityDisplayed && !quantityDisplayed) {
      rejectValue(errors, COLUMNS_MAP,
          new Message(ERROR_DISPLAYED_WHEN_CALC_ORDER_QUANTITY_EXPLANATION_NOT_DISPLAYED,
              REQUESTED_QUANTITY));
    }
  }

  private void validateCalculatedFields(Errors errors, RequisitionTemplate template) {
    if (template.isColumnCalculated(TOTAL_CONSUMED_QUANTITY)
        && template.isColumnCalculated(STOCK_ON_HAND)) {
      rejectValue(errors, COLUMNS_MAP, new Message(ERROR_CANNOT_CALCULATE_AT_THE_SAME_TIME,
          TOTAL_CONSUMED_QUANTITY, STOCK_ON_HAND));
    }
  }

  private void validateCalculatedField(Errors errors, RequisitionTemplate template, String field,
                                       String suffix, String... requiredFields) {
    if (template.isColumnCalculated(field)) {
      for (String requiredField : requiredFields) {
        if (!template.isColumnInTemplate(requiredField)) {
          rejectValue(
              errors, COLUMNS_MAP,
              new Message(ERROR_VALIDATION_FIELD_MUST_BE_IN_TEMPLATE, requiredField, field)
          );
        }
        if (template.isColumnUserInput(requiredField)) {
          rejectIfNotDisplayed(errors, template, requiredField, COLUMNS_MAP,
              new Message(suffix, requiredField));
        }
      }
    }
  }

  private void validateColumns(Errors errors, RequisitionTemplate template) {
    for (RequisitionTemplateColumn column : template.getColumnsMap().values()) {
      rejectIfNotAlphanumeric(
          errors, column.getLabel(), COLUMNS_MAP,
          new Message(ERROR_ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED, column.getName())
      );

      validateColumnDefinition(errors, column);
      validateChosenSources(errors, template, column);

      Set<AvailableRequisitionColumnOption> options = column.getColumnDefinition().getOptions();
      Optional.ofNullable(column.getOption())
          .ifPresent(option -> rejectIfNotContains(errors, options, option, COLUMNS_MAP,
              new Message(ERROR_OPTION_NOT_AVAILABLE, option.toString())));

      rejectIfLengthTooLong(
          errors, column.getDefinition(), MAX_COLUMN_DEFINITION_LENGTH, COLUMNS_MAP,
          new Message(ERROR_VALIDATION_FIELD_IS_TOO_LONG, DEFINITION, MAX_COLUMN_DEFINITION_LENGTH)
      );
    }
  }

  private void validateColumnDefinition(Errors errors, RequisitionTemplateColumn column) {
    AvailableRequisitionColumn actual = column.getColumnDefinition();
    AvailableRequisitionColumn expected = availableRequisitionColumnRepository
        .findOne(actual.getId());

    if (null == expected) {
      rejectValue(
          errors, COLUMNS_MAP,
          new Message(ERROR_VALIDATION_COLUMN_DEFINITION_NOT_FOUND, column.getLabel())
      );
    } else if (!expected.equals(actual)) {
      rejectValue(
          errors, COLUMNS_MAP,
          new Message(ERROR_VALIDATION_COLUMN_DEFINITION_MODIFIED, column.getLabel())
      );
    }
  }

  private void validateChosenSources(Errors errors, RequisitionTemplate template,
                                     RequisitionTemplateColumn column) {
    SourceType chosenSource = column.getSource();

    if (chosenSource == null) {
      rejectValue(errors, COLUMNS_MAP,
          new Message(ERROR_SOURCE_OF_REQUISITION_TEMPLATE_COLUMN_CANNOT_BE_NULL,
              column.getLabel()));
    } else {
      AvailableRequisitionColumn definition = column.getColumnDefinition();
      Set<SourceType> sources = definition.getSources();

      if (sources.size() > 1 && template.isColumnUserInput(definition.getName())) {
        rejectIfNotDisplayed(
            errors, template, definition.getName(), COLUMNS_MAP,
            new Message(ERROR_MUST_BE_DISPLAYED, definition.getName())
        );
      }
      rejectIfNotContains(
          errors, sources, chosenSource, COLUMNS_MAP,
          new Message(ERROR_SOURCE_NOT_AVAILABLE, chosenSource.toString())
      );
    }
  }

  

}
