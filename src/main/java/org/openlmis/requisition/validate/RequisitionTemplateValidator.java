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

import static org.openlmis.requisition.domain.RequisitionTemplateColumn.DEFINITION_KEY;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_CANNOT_CALCULATE_AT_THE_SAME_TIME;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DISPLAYED_WHEN_CALC_ORDER_QUANTITY_EXPLANATION_NOT_DISPLAYED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DISPLAYED_WHEN_REQUESTED_QUANTITY_EXPLANATION_IS_DISPLAYED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_DISPLAYED_WHEN_REQUESTED_QUANTITY_IS_DISPLAYED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_DISPLAYED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_DISPLAYED_WHEN_AVERAGE_CONSUMPTION_IS_CALCULATED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMED_QUANTITY_IS_CALCULATED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_DISPLAYED_WHEN_ON_HAND_IS_CALCULATED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_NOT_BE_DISPLAYED_WHEN_SOH_POPULATED_FROM_STOCK_CARDS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_OPTION_NOT_AVAILABLE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_COLUMN_SOURCE_INVALID;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SOURCE_NOT_AVAILABLE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SOURCE_OF_REQUISITION_TEMPLATE_COLUMN_CANNOT_BE_NULL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_COLUMN_DEFINITION_MODIFIED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_COLUMN_DEFINITION_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_FIELD_CANNOT_BE_NULL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_FIELD_IS_TOO_LONG;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_FIELD_MUST_BE_GREATER_OR_EQUAL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_FIELD_MUST_BE_IN_TEMPLATE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_REFERENCED_OBJECT_DOES_NOT_EXIST;

import org.javers.common.collections.Sets;
import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.AvailableRequisitionColumnOption;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.repository.AvailableRequisitionColumnRepository;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

@Component
@SuppressWarnings("PMD.TooManyMethods")
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
  static final String BEGINNING_BALANCE = "beginningBalance";
  static final String TOTAL_RECEIVED_QUANTITY = "totalReceivedQuantity";
  static final String TOTAL = "total";
  static final String NUMBER_OF_NEW_PATIENTS_ADDED = "numberOfNewPatientsAdded";
  static final String MAXIMUM_STOCK_QUANTITY = "maximumStockQuantity";
  static final int MAX_COLUMN_DEFINITION_LENGTH = 140;
  static final Set<String> STOCK_DISABLED_COLUMNS = Sets.asSet(
      BEGINNING_BALANCE, TOTAL_RECEIVED_QUANTITY, TOTAL_CONSUMED_QUANTITY,
      TOTAL_STOCKOUT_DAYS, TOTAL, NUMBER_OF_NEW_PATIENTS_ADDED, ADJUSTED_CONSUMPTION,
      MAXIMUM_STOCK_QUANTITY, CALCULATED_ORDER_QUANTITY);

  private Errors errors;

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
    this.errors = errors;

    RequisitionTemplate requisitionTemplate = (RequisitionTemplate) target;

    validateRequestedQuantity(requisitionTemplate);
    validateColumns(requisitionTemplate);
    validateCalculatedFields(requisitionTemplate);

    if (!errors.hasErrors()) {
      validateCalculatedField(requisitionTemplate, STOCK_ON_HAND,
          ERROR_MUST_BE_DISPLAYED_WHEN_ON_HAND_IS_CALCULATED, TOTAL_CONSUMED_QUANTITY
      );
      validateCalculatedField(requisitionTemplate, TOTAL_CONSUMED_QUANTITY,
          ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMED_QUANTITY_IS_CALCULATED, STOCK_ON_HAND
      );
      validateForAdjustedConsumption(requisitionTemplate);
      validateForAverageConsumption(requisitionTemplate);
    }

    validateNumberOfPeriodsToAverage(requisitionTemplate);

    if (requisitionTemplate.isPopulateStockOnHandFromStockCards()) {
      validateStockManagementFields(requisitionTemplate);
    }

    UUID programId = requisitionTemplate.getProgramId();
    if (null != programId && null == programReferenceDataService.findOne(programId)) {
      rejectValue(errors, PROGRAM_ID,
          new Message(ERROR_VALIDATION_REFERENCED_OBJECT_DOES_NOT_EXIST,
              PROGRAM, programId));
    }
  }

  private void validateRequestedQuantity(RequisitionTemplate template) {
    boolean quantityDisplayed = template.isColumnDisplayed(REQUESTED_QUANTITY);
    boolean explanationDisplayed = template.isColumnDisplayed(REQUESTED_QUANTITY_EXPLANATION);
    boolean calcOrderQuantityDisplayed = template.isColumnDisplayed(CALCULATED_ORDER_QUANTITY);

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

  private void validateCalculatedFields(RequisitionTemplate template) {
    if (template.isColumnInTemplate(TOTAL_CONSUMED_QUANTITY)
        && template.isColumnCalculated(TOTAL_CONSUMED_QUANTITY)
        && template.isColumnInTemplate(STOCK_ON_HAND)
        && template.isColumnCalculated(STOCK_ON_HAND)) {
      rejectValue(errors, COLUMNS_MAP, new Message(ERROR_CANNOT_CALCULATE_AT_THE_SAME_TIME,
          TOTAL_CONSUMED_QUANTITY, STOCK_ON_HAND));
    }
  }

  private void validateCalculatedField(RequisitionTemplate template, String field,
                                       String suffix, String... requiredFields) {
    if (template.isColumnInTemplate(field) && template.isColumnCalculated(field)) {
      for (String requiredField : requiredFields) {
        if (template.isColumnInTemplate(requiredField)
            && template.isColumnUserInput(requiredField)) {
          rejectIfNotDisplayed(errors, template, requiredField, COLUMNS_MAP,
              new Message(suffix, requiredField));
        }
      }
    }
  }

  private void validateColumns(RequisitionTemplate template) {
    for (RequisitionTemplateColumn column : template.getColumnsMap().values()) {
      rejectIfNotAlphanumeric(
          errors, column.getLabel(), COLUMNS_MAP,
          new Message(ERROR_ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED, column.getName())
      );

      validateColumnDefinition(column);
      validateChosenSources(template, column);

      Set<AvailableRequisitionColumnOption> options = column.getColumnDefinition().getOptions();
      Optional.ofNullable(column.getOption())
          .ifPresent(option -> rejectIfNotContains(errors, options, option, COLUMNS_MAP,
              new Message(ERROR_OPTION_NOT_AVAILABLE, option.toString())));

      rejectIfLengthTooLong(
          errors, column.getDefinition(), MAX_COLUMN_DEFINITION_LENGTH, COLUMNS_MAP,
          new Message(ERROR_VALIDATION_FIELD_IS_TOO_LONG, DEFINITION_KEY,
              MAX_COLUMN_DEFINITION_LENGTH)
      );
    }
  }

  private void validateColumnDefinition(RequisitionTemplateColumn column) {
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

  private void validateChosenSources(RequisitionTemplate template,
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

  private void validateStockManagementFields(RequisitionTemplate requisitionTemplate) {
    if (requisitionTemplate.isColumnInTemplate(STOCK_ON_HAND)) {
      RequisitionTemplateColumn soh = requisitionTemplate.getColumnsMap().get(STOCK_ON_HAND);
      if (!SourceType.STOCK_CARDS.equals(soh.getSource())) {
        rejectValue(errors, COLUMNS_MAP,
            new Message(ERROR_COLUMN_SOURCE_INVALID, STOCK_ON_HAND, SourceType.STOCK_CARDS,
                soh.getSource()));
      }
    }

    for (String columnName : STOCK_DISABLED_COLUMNS) {
      if (requisitionTemplate.isColumnInTemplate(columnName)
          && requisitionTemplate.findColumn(columnName).getIsDisplayed()) {
        rejectIfDisplayed(errors, requisitionTemplate, columnName, COLUMNS_MAP, new Message(
            ERROR_MUST_NOT_BE_DISPLAYED_WHEN_SOH_POPULATED_FROM_STOCK_CARDS, columnName));
      }
    }
  }

  private void validateForAdjustedConsumption(RequisitionTemplate requisitionTemplate) {
    if (requisitionTemplate.isColumnInTemplate(ADJUSTED_CONSUMPTION)) {
      rejectIfStockoutDaysOrConsumedQuantityNotInTemplate(
          requisitionTemplate, ADJUSTED_CONSUMPTION);
      if (requisitionTemplate.isColumnDisplayed(ADJUSTED_CONSUMPTION)) {
        validateCalculatedField(requisitionTemplate, ADJUSTED_CONSUMPTION,
            ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED, TOTAL_CONSUMED_QUANTITY,
            TOTAL_STOCKOUT_DAYS);
      }
    }
  }

  private void validateForAverageConsumption(RequisitionTemplate requisitionTemplate) {
    if (requisitionTemplate.isColumnInTemplate(AVERAGE_CONSUMPTION)) {
      rejectIfStockoutDaysOrConsumedQuantityNotInTemplate(requisitionTemplate, AVERAGE_CONSUMPTION);
      if (requisitionTemplate.isColumnDisplayed(AVERAGE_CONSUMPTION)) {
        validateCalculatedField(requisitionTemplate, AVERAGE_CONSUMPTION,
            ERROR_MUST_BE_DISPLAYED_WHEN_AVERAGE_CONSUMPTION_IS_CALCULATED, TOTAL_CONSUMED_QUANTITY,
            TOTAL_STOCKOUT_DAYS);
      }
      rejectIfNumberOfPeriodsToAverageIsNull(requisitionTemplate);
    }
  }

  private void rejectIfStockoutDaysOrConsumedQuantityNotInTemplate(
      RequisitionTemplate requisitionTemplate, String validatedColumn) {
    if (!requisitionTemplate.isColumnInTemplate(TOTAL_CONSUMED_QUANTITY)) {
      rejectValue(errors, COLUMNS_MAP,
          new Message(ERROR_VALIDATION_FIELD_MUST_BE_IN_TEMPLATE,
              TOTAL_CONSUMED_QUANTITY, validatedColumn));
    }
    if (!requisitionTemplate.isColumnInTemplate(TOTAL_STOCKOUT_DAYS)) {
      rejectValue(errors, COLUMNS_MAP,
          new Message(ERROR_VALIDATION_FIELD_MUST_BE_IN_TEMPLATE,
              TOTAL_STOCKOUT_DAYS, validatedColumn));
    }
  }


  private void rejectIfNumberOfPeriodsToAverageIsNull(RequisitionTemplate requisitionTemplate) {
    if (requisitionTemplate.getNumberOfPeriodsToAverage() == null) {
      rejectValue(errors, NUMBER_OF_PERIODS_TO_AVERAGE,
          new Message(ERROR_VALIDATION_FIELD_CANNOT_BE_NULL,
              NUMBER_OF_PERIODS_TO_AVERAGE, AVERAGE_CONSUMPTION));
    }
  }

  private void validateNumberOfPeriodsToAverage(RequisitionTemplate requisitionTemplate) {
    if (requisitionTemplate.getNumberOfPeriodsToAverage() != null
        && requisitionTemplate.getNumberOfPeriodsToAverage() < 2) {
      rejectValue(errors, NUMBER_OF_PERIODS_TO_AVERAGE,
          new Message(ERROR_VALIDATION_FIELD_MUST_BE_GREATER_OR_EQUAL,
              NUMBER_OF_PERIODS_TO_AVERAGE, "2"));
    }
  }
}
