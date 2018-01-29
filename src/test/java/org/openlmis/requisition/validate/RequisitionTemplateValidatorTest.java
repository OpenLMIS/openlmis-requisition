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

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.contains;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.RequisitionTemplateColumn.COLUMN_DEFINITION;
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
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SOH_STOCK_CARDS_SOURCE_REQUIRED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SOURCE_NOT_AVAILABLE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SOURCE_OF_REQUISITION_TEMPLATE_COLUMN_CANNOT_BE_NULL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_COLUMN_DEFINITION_MODIFIED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_COLUMN_DEFINITION_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_FIELD_IS_TOO_LONG;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_FIELD_MUST_BE_IN_TEMPLATE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_REFERENCED_OBJECT_DOES_NOT_EXIST;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.ADJUSTED_CONSUMPTION;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.AVERAGE_CONSUMPTION;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.CALCULATED_ORDER_QUANTITY;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.COLUMNS_MAP;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.MAX_COLUMN_DEFINITION_LENGTH;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.NUMBER_OF_PERIODS_TO_AVERAGE;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.PROGRAM;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.PROGRAM_ID;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.REQUESTED_QUANTITY;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.REQUESTED_QUANTITY_EXPLANATION;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.STOCK_DISABLED_COLUMNS;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.STOCK_ON_HAND;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.TOTAL_CONSUMED_QUANTITY;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.TOTAL_STOCKOUT_DAYS;

import org.apache.commons.lang.RandomStringUtils;
import org.javers.common.collections.Sets;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.AvailableRequisitionColumnOption;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.repository.AvailableRequisitionColumnRepository;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.testutils.AvailableRequisitionColumnDataBuilder;
import org.openlmis.requisition.testutils.ProgramDtoDataBuilder;
import org.openlmis.requisition.testutils.RequisitionTemplateColumnDataBuilder;
import org.openlmis.requisition.testutils.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.utils.Message;
import org.springframework.validation.Errors;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
@RunWith(MockitoJUnitRunner.class)
public class RequisitionTemplateValidatorTest {

  private static final String COLUMN_NAME = "test";
  private static final String ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED =
      "only alphanumeric label is accepted";

  @Mock
  private MessageService messageService;

  @Mock
  private AvailableRequisitionColumnRepository availableRequisitionColumnRepository;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @InjectMocks
  private RequisitionTemplateValidator validator;

  private Errors errors = mock(Errors.class);
  private Message.LocalizedMessage message =
      new Message("testKey").new LocalizedMessage("testMessage");

  @Before
  public void prepareMessageServiceMock() {
    Message message1 = new Message(
        ERROR_DISPLAYED_WHEN_REQUESTED_QUANTITY_IS_DISPLAYED,
        REQUESTED_QUANTITY_EXPLANATION);
    Message message2 = new Message(
        ERROR_DISPLAYED_WHEN_REQUESTED_QUANTITY_EXPLANATION_IS_DISPLAYED,
        REQUESTED_QUANTITY);
    Message message3 = new Message(
        ERROR_CANNOT_CALCULATE_AT_THE_SAME_TIME,
        TOTAL_CONSUMED_QUANTITY,
        STOCK_ON_HAND);
    Message message7 = new Message(
        ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMED_QUANTITY_IS_CALCULATED,
        STOCK_ON_HAND);
    Message message8 = new Message(
        ERROR_MUST_BE_DISPLAYED_WHEN_ON_HAND_IS_CALCULATED,
        TOTAL_CONSUMED_QUANTITY);
    Message message9 = new Message(
        ERROR_DISPLAYED_WHEN_CALC_ORDER_QUANTITY_EXPLANATION_NOT_DISPLAYED,
        CALCULATED_ORDER_QUANTITY);

    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        REQUESTED_QUANTITY_EXPLANATION + " must be displayed when "
            + "requested quantity is displayed."));
    when(messageService.localize(message2)).thenReturn(message2.new LocalizedMessage(
        REQUESTED_QUANTITY + " must be "
            + "displayed when requested quantity explanation is displayed."));
    when(messageService.localize(message3)).thenReturn(message3.new LocalizedMessage(
        TOTAL_CONSUMED_QUANTITY + " and "
            + STOCK_ON_HAND
            + " cannot be calculated at the same time."));
    when(messageService.localize(message7)).thenReturn(message7.new LocalizedMessage(
        "must be displayed when total consumed quantity is calculated."));
    when(messageService.localize(message8)).thenReturn(message8.new LocalizedMessage(
        "must be displayed when stock on hand is calculated."));
    when(messageService.localize(message9)).thenReturn(message9.new LocalizedMessage(
        REQUESTED_QUANTITY
            + " must be displayed when calculated order quantity is not displayed."));
  }

  @Test
  public void shouldRejectIfRequestedQuantityAndExplanationIsDisplayedValuesAreDifferent() {
    RequisitionTemplate requisitionTemplate = generateTemplate();
    requisitionTemplate.changeColumnDisplay(
        REQUESTED_QUANTITY_EXPLANATION, false);
    requisitionTemplate.changeColumnSource(REQUESTED_QUANTITY,
        SourceType.USER_INPUT);

    requisitionTemplate.changeColumnSource(
        REQUESTED_QUANTITY_EXPLANATION, SourceType.USER_INPUT);

    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP),
        contains(REQUESTED_QUANTITY_EXPLANATION
            + " must be displayed when requested quantity is displayed."));
  }

  @Test
  public void shouldRejectWhenRequestedQuantityAndCalcOrderQuantityAreNotDisplayed() {
    RequisitionTemplate requisitionTemplate = generateTemplate();
    requisitionTemplate.changeColumnDisplay(REQUESTED_QUANTITY, false);
    requisitionTemplate.changeColumnDisplay(REQUESTED_QUANTITY_EXPLANATION, false);
    requisitionTemplate.changeColumnDisplay(CALCULATED_ORDER_QUANTITY, false);

    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP),
        contains(REQUESTED_QUANTITY
            + " must be displayed when calculated order quantity is not displayed."));
  }

  @Test
  public void shouldRejectWhenSourceIsNotAvailableInColumn() {
    RequisitionTemplate requisitionTemplate = generateTemplate();

    requisitionTemplate.changeColumnSource(COLUMN_NAME, SourceType.USER_INPUT);
    requisitionTemplate.getColumnsMap()
        .get(COLUMN_NAME).getColumnDefinition().getSources().clear();

    Message message4 = new Message(ERROR_SOURCE_NOT_AVAILABLE, "test");
    when(messageService.localize(message4)).thenReturn(message4.new LocalizedMessage(
        "Source " + SourceType.USER_INPUT + " is not available for this column."));

    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP),
        contains(RequisitionTemplate.SOURCE + SourceType.USER_INPUT
            + RequisitionTemplate.WARNING_SUFFIX));
  }

  @Test
  public void shouldRejectWhenOptionIsNotAvailableInColumn() {
    RequisitionTemplate requisitionTemplate = generateTemplate();
    AvailableRequisitionColumnOption option = new AvailableRequisitionColumnOption(
        requisitionTemplate.getColumnsMap().get(COLUMN_NAME)
            .getColumnDefinition(), "option1", "label1");
    Set<AvailableRequisitionColumnOption> options = new HashSet<>();
    options.add(option);
    requisitionTemplate.getColumnsMap().get(COLUMN_NAME).getColumnDefinition().setOptions(options);
    requisitionTemplate.changeColumnOption(COLUMN_NAME, option);

    requisitionTemplate.getColumnsMap().get(COLUMN_NAME)
        .getColumnDefinition().getOptions().clear();

    Message message5 = new Message(ERROR_OPTION_NOT_AVAILABLE, "test");
    when(messageService.localize(message5)).thenReturn(message5.new LocalizedMessage(
        "Option " + option.getOptionName() + " is not available for this column."));

    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP),
        contains(RequisitionTemplate.OPTION + option.getOptionName()
            + RequisitionTemplate.WARNING_SUFFIX));
  }

  @Test
  public void shouldNotRejectWhenConsumptionsInTemplateAndStockoutDaysInTemplate() {
    RequisitionTemplate template = addAdjustedConsumptionToColumnsMapAndGetRequisitionTemplate();
    RequisitionTemplateColumn column = new RequisitionTemplateColumnDataBuilder()
        .withName(TOTAL_STOCKOUT_DAYS)
        .withIndicator("X")
        .withColumnDefinition(new AvailableRequisitionColumnDataBuilder()
            .withName(TOTAL_STOCKOUT_DAYS)
            .withSources(Sets.asSet(SourceType.USER_INPUT))
            .build())
        .withSource(SourceType.USER_INPUT)
        .build();
    template.getColumnsMap().put(TOTAL_STOCKOUT_DAYS, column);

    when(availableRequisitionColumnRepository.findOne(column.getColumnDefinition().getId()))
        .thenReturn(column.getColumnDefinition());

    validator.validate(template, errors);

    verify(errors, never()).rejectValue(anyString(), anyString());
  }

  @Test
  public void shouldRejectWhenConsumptionsInTemplateAndStockoutDaysNotInTemplate() {
    when(messageService.localize(
        new Message(ERROR_VALIDATION_FIELD_MUST_BE_IN_TEMPLATE)))
        .thenReturn(message);

    RequisitionTemplate template = addAdjustedConsumptionToColumnsMapAndGetRequisitionTemplate();

    validator.validate(template, errors);

    verify(errors, times(2)).rejectValue(COLUMNS_MAP, message.toString());
  }

  @Test
  public void shouldNotRejectWhenConsumptionsInTemplateAndConsumedQuantityInTemplate() {
    RequisitionTemplate template = addAdjustedConsumptionToColumnsMapAndGetRequisitionTemplate();
    RequisitionTemplateColumn column = new RequisitionTemplateColumnDataBuilder()
        .withName(TOTAL_STOCKOUT_DAYS)
        .withIndicator("X")
        .withColumnDefinition(new AvailableRequisitionColumnDataBuilder()
            .withName(TOTAL_STOCKOUT_DAYS)
            .withSources(Sets.asSet(SourceType.USER_INPUT))
            .build())
        .withSource(SourceType.USER_INPUT)
        .build();
    template.getColumnsMap().put(TOTAL_STOCKOUT_DAYS, column);

    when(availableRequisitionColumnRepository.findOne(column.getColumnDefinition().getId()))
        .thenReturn(column.getColumnDefinition());

    validator.validate(template, errors);

    verify(errors, never()).rejectValue(anyString(), anyString());
  }

  @Test
  public void shouldRejectWhenConsumptionsInTemplateAnConsumedQuantityNotInTemplate() {
    when(messageService.localize(
        new Message(ERROR_VALIDATION_FIELD_MUST_BE_IN_TEMPLATE)))
        .thenReturn(message);

    RequisitionTemplate template = addAdjustedConsumptionToColumnsMapAndGetRequisitionTemplate();

    validator.validate(template, errors);

    verify(errors, times(2)).rejectValue(COLUMNS_MAP, message.toString());
  }

  @Test
  public void shouldRejectWhenAdjustedConsumptionIsDisplayedAndStockoutDaysIsNotDisplayed() {
    RequisitionTemplate requisitionTemplate = mockMessageAndGetRequisitionTemplate(
        ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED);
    requisitionTemplate.changeColumnDisplay(TOTAL_STOCKOUT_DAYS, false);
    requisitionTemplate.changeColumnDisplay(AVERAGE_CONSUMPTION, false);
    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(COLUMNS_MAP, message.toString());
  }

  @Test
  public void shouldRejectWhenAverageConsumptionIsDisplayedAndStockoutDaysIsNotDisplayed() {
    RequisitionTemplate requisitionTemplate = mockMessageAndGetRequisitionTemplate(
        ERROR_MUST_BE_DISPLAYED_WHEN_AVERAGE_CONSUMPTION_IS_CALCULATED);
    requisitionTemplate.changeColumnDisplay(TOTAL_STOCKOUT_DAYS, false);
    requisitionTemplate.changeColumnDisplay(ADJUSTED_CONSUMPTION, false);
    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(COLUMNS_MAP, message.toString());
  }

  @Test
  public void shouldNotRejectWhenConsumptionsAreDisplayedAndStockoutDaysIsDisplayed() {
    RequisitionTemplate requisitionTemplate = mockMessageAndGetRequisitionTemplate(
        ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED);
    requisitionTemplate.changeColumnDisplay(TOTAL_STOCKOUT_DAYS, true);
    validator.validate(requisitionTemplate, errors);

    verify(errors, never()).rejectValue(anyString(), anyString());
  }

  @Test
  public void shouldNotRejectWhenConsumptionsAreNotDisplayedAndStockoutDaysIsNotDisplayed() {
    RequisitionTemplate requisitionTemplate = mockMessageAndGetRequisitionTemplate(
        ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED);
    requisitionTemplate.changeColumnDisplay(AVERAGE_CONSUMPTION, false);
    requisitionTemplate.changeColumnDisplay(ADJUSTED_CONSUMPTION, false);
    requisitionTemplate.changeColumnDisplay(TOTAL_STOCKOUT_DAYS, false);
    validator.validate(requisitionTemplate, errors);

    verify(errors, never()).rejectValue(anyString(), anyString());
  }

  @Test
  public void shouldNotRejectWhenConsumptionsAreDisplayedAndConsumedQuantityIsCalculated() {
    RequisitionTemplate requisitionTemplate = mockMessageAndGetRequisitionTemplate(
        ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED);
    requisitionTemplate.changeColumnSource(TOTAL_CONSUMED_QUANTITY, SourceType.CALCULATED);
    requisitionTemplate.changeColumnDisplay(TOTAL_CONSUMED_QUANTITY, false);
    validator.validate(requisitionTemplate, errors);

    verify(errors, never()).rejectValue(anyString(), anyString());
  }

  @Test
  public void shouldNotRejectWhenConsumptionsAreDisplayedAndConsumedQuantityIsDisplayed() {
    RequisitionTemplate requisitionTemplate = mockMessageAndGetRequisitionTemplate(
        ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED);
    requisitionTemplate.changeColumnDisplay(TOTAL_CONSUMED_QUANTITY, true);
    validator.validate(requisitionTemplate, errors);

    verify(errors, never()).rejectValue(anyString(), anyString());
  }

  @Test
  public void shouldNotRejectWhenConsumptionsAndConsumedQuantityAreNotDisplayed() {
    RequisitionTemplate requisitionTemplate = mockMessageAndGetRequisitionTemplate(
        ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED);
    requisitionTemplate.changeColumnSource(TOTAL_CONSUMED_QUANTITY, SourceType.CALCULATED);
    requisitionTemplate.changeColumnDisplay(AVERAGE_CONSUMPTION, false);
    requisitionTemplate.changeColumnDisplay(ADJUSTED_CONSUMPTION, false);
    requisitionTemplate.changeColumnDisplay(TOTAL_CONSUMED_QUANTITY, false);
    validator.validate(requisitionTemplate, errors);

    verify(errors, never()).rejectValue(anyString(), anyString());
  }

  @Test
  public void shouldRejectWhenAverageInTemplateAndNumberOfPreviousPeriodsIsNull() {
    when(messageService.localize(
        new Message("requisition.error.validation.fieldCanNotBeNull")))
        .thenReturn(message);

    RequisitionTemplate requisitionTemplate
        = getRequisitionTemplateForTestAdjustedAndAverageConsumptionField();
    requisitionTemplate.setNumberOfPeriodsToAverage(null);

    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(NUMBER_OF_PERIODS_TO_AVERAGE, message.toString());
  }

  @Test
  public void shouldRejectWhenNumberOfPreviousPeriodsLessThanTwo() {
    when(messageService.localize(
        new Message("requisition.error.validation.fieldMustBeGreaterOrEqual")))
        .thenReturn(message);

    RequisitionTemplate template = baseTemplateBuilder()
        .withNumberOfPeriodsToAverage(1)
        .build();
    mockResponses(template);

    validator.validate(template, errors);

    verify(errors).rejectValue(NUMBER_OF_PERIODS_TO_AVERAGE, message.toString());
  }

  @Test
  public void shouldNotRejectWhenNumberOfPreviousPeriodsGreaterOrEqualThanTwo() {
    RequisitionTemplate template = baseTemplateBuilder()
        .withNumberOfPeriodsToAverage(2)
        .build();

    mockResponses(template);

    validator.validate(template, errors);

    verify(errors, never()).rejectValue(eq(NUMBER_OF_PERIODS_TO_AVERAGE), any());
  }

  @Test
  public void shouldRejectIfColumnIsNotDisplayedHasUserInputSourceAndSeveralAvailableSources() {
    Message message = new Message(ERROR_MUST_BE_DISPLAYED, STOCK_ON_HAND);
    String errorMessage = "stockOnHand must be displayed";

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        errorMessage
    ));

    RequisitionTemplate template = baseTemplateBuilder()
        .withNumberOfPeriodsToAverage(2)
        .build();

    template.getColumnsMap().get(STOCK_ON_HAND).setIsDisplayed(false);
    template.getColumnsMap().get(STOCK_ON_HAND).setSource(SourceType.USER_INPUT);

    mockResponses(template);

    validator.validate(template, errors);

    verify(errors, never()).rejectValue(COLUMNS_MAP, errorMessage);
  }

  @Test
  public void shouldRejectIfColumnDefinitionIsTooLong() throws Exception {
    Message message = new Message(ERROR_VALIDATION_FIELD_IS_TOO_LONG,
        DEFINITION_KEY, MAX_COLUMN_DEFINITION_LENGTH);
    String errorMessage = DEFINITION_KEY
        + " is too long. The maximum length is "
        + MAX_COLUMN_DEFINITION_LENGTH;

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    RequisitionTemplate template = baseTemplateBuilder()
        .withNumberOfPeriodsToAverage(2)
        .build();
    template.getColumnsMap().get(STOCK_ON_HAND).setDefinition(RandomStringUtils.random(200));
    mockResponses(template);

    validator.validate(template, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfColumnDefinitionCannotBeFound() throws Exception {
    Message message = new Message(ERROR_VALIDATION_COLUMN_DEFINITION_NOT_FOUND,
        COLUMN_DEFINITION, "mocked");
    String errorMessage = "Cannot find column definition for column mocked";

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    RequisitionTemplate template = generateTemplate();

    when(availableRequisitionColumnRepository.findOne(template.getColumnsMap().get(STOCK_ON_HAND)
        .getColumnDefinition().getId())).thenReturn(null);

    validator.validate(template, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfColumnDefinitionWasModified() throws Exception {
    Message message = new Message(ERROR_VALIDATION_COLUMN_DEFINITION_MODIFIED,
        COLUMN_DEFINITION, "mocked");
    String errorMessage = "Cannot find column definition for column mocked";

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    RequisitionTemplate template = generateTemplate();

    when(availableRequisitionColumnRepository
        .findOne(template.getColumnsMap().get(STOCK_ON_HAND).getColumnDefinition().getId()))
        .thenReturn(template.getColumnsMap().get(REQUESTED_QUANTITY).getColumnDefinition());

    validator.validate(template, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfColumnLabelIsInvalid() {
    Message message = new Message(ERROR_ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED);
    String errorMessage = ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED;

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    RequisitionTemplate template = generateTemplate();

    template.getColumnsMap().get(STOCK_ON_HAND)
        .setLabel("New not valid name with wrong signs: !@#$%^&*()");
    validator.validate(new RequisitionTemplate(template.getColumnsMap()), errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfColumnLabelNameHasSpecialCharacters() {
    Message message = new Message(ERROR_ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED);
    String errorMessage = ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED;

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    RequisitionTemplate template = generateTemplate();

    template.getColumnsMap().get(STOCK_ON_HAND).setLabel(")(*&^%$#@!");
    validator.validate(template, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfColumnLabelIsNull() {
    Message message = new Message(ERROR_ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED);
    String errorMessage = ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED;

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    RequisitionTemplate template = generateTemplate();

    template.getColumnsMap().get(STOCK_ON_HAND).setLabel(null);
    validator.validate(template, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfColumnLabelIsEmpty() {
    Message message = new Message(ERROR_ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED);
    String errorMessage = ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED;

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    RequisitionTemplate template = generateTemplate();

    template.getColumnsMap().get(STOCK_ON_HAND).setLabel("");
    validator.validate(template, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfColumnLabelNameHasOnlyWhiteSpace() {
    Message message = new Message(ERROR_ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED);
    String errorMessage = ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED;

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    RequisitionTemplate template = generateTemplate();

    template.getColumnsMap().get(STOCK_ON_HAND).setLabel(" ");
    validator.validate(template, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfProgramWithSpecifiedIdDoesNotExist() throws Exception {
    UUID programId = UUID.randomUUID();
    when(programReferenceDataService.findOne(programId)).thenReturn(null);

    RequisitionTemplate requisitionTemplate = generateTemplate();
    requisitionTemplate.setProgramId(programId);

    Message message = new Message(ERROR_VALIDATION_REFERENCED_OBJECT_DOES_NOT_EXIST,
        PROGRAM, programId);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        message.toString()));

    validator.validate(requisitionTemplate, errors);
    verify(errors).rejectValue(eq(PROGRAM_ID), contains(message.toString()));
  }

  @Test
  public void shouldRejectWhenSourceInRequisitionTemplateColumnIsNull() throws Exception {
    RequisitionTemplate requisitionTemplate = generateTemplate();
    requisitionTemplate.findColumn(REQUESTED_QUANTITY_EXPLANATION).setSource(null);

    Message message = new Message(ERROR_SOURCE_OF_REQUISITION_TEMPLATE_COLUMN_CANNOT_BE_NULL,
        REQUESTED_QUANTITY_EXPLANATION);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        message.toString()));

    validator.validate(requisitionTemplate, errors);
    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(message.toString()));
  }

  @Test
  public void shouldRejectWhenSohSourceIsOtherThanStockCards() throws Exception {
    RequisitionTemplate template = getTemplatePopulatedByStock();

    validator.validate(template, errors);

    Message message = new Message(ERROR_SOH_STOCK_CARDS_SOURCE_REQUIRED);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        message.toString()));

    template.findColumn(STOCK_ON_HAND).setSource(SourceType.CALCULATED);

    validator.validate(template, errors);
    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(message.toString()));
  }

  @Test
  public void shouldRejectWhenStockDisabledColumnIsDisplayed() throws Exception {
    RequisitionTemplate template = getTemplatePopulatedByStock();

    validator.validate(template, errors);

    Message message = new Message(ERROR_MUST_NOT_BE_DISPLAYED_WHEN_SOH_POPULATED_FROM_STOCK_CARDS);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        message.toString()));

    template.findColumn(TOTAL_CONSUMED_QUANTITY).setIsDisplayed(true);

    validator.validate(template, errors);
    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(message.toString()));
  }

  private RequisitionTemplateDataBuilder baseTemplateBuilder() {
    return new RequisitionTemplateDataBuilder()
        .withRequiredColumns()
        .withColumn(TOTAL_CONSUMED_QUANTITY, "C", SourceType.USER_INPUT,
            Sets.asSet(SourceType.USER_INPUT, SourceType.CALCULATED))
        .withColumn(STOCK_ON_HAND, "E", SourceType.USER_INPUT,
            Sets.asSet(SourceType.USER_INPUT, SourceType.CALCULATED))
        .withColumn(COLUMN_NAME, "T", SourceType.USER_INPUT,
            Sets.asSet(SourceType.USER_INPUT));
  }

  private RequisitionTemplate generateTemplate() {
    RequisitionTemplate template = baseTemplateBuilder().build();
    mockResponses(template);
    return template;
  }

  private RequisitionTemplate getTemplatePopulatedByStock() {
    RequisitionTemplate template = baseTemplateBuilder()
        .withPopulateStockOnHandFromStockCards()
        .build();
    template.findColumn(STOCK_ON_HAND).getColumnDefinition()
        .getSources().add(SourceType.STOCK_CARDS);
    template.findColumn(STOCK_ON_HAND).setSource(SourceType.STOCK_CARDS);
    template.findColumn(TOTAL_CONSUMED_QUANTITY).setSource(SourceType.CALCULATED);

    STOCK_DISABLED_COLUMNS.stream()
        .filter(template::isColumnInTemplate)
        .forEach(c -> template.changeColumnDisplay(c, false));

    mockResponses(template);

    return template;
  }

  private RequisitionTemplate addAdjustedConsumptionToColumnsMapAndGetRequisitionTemplate() {
    RequisitionTemplate template = baseTemplateBuilder()
        .withColumn(TOTAL_CONSUMED_QUANTITY, "X", SourceType.USER_INPUT,
            Sets.asSet(SourceType.USER_INPUT))
        .withColumn(ADJUSTED_CONSUMPTION, "N", SourceType.CALCULATED,
            Sets.asSet(SourceType.CALCULATED))
        .withColumn(AVERAGE_CONSUMPTION, "P", SourceType.CALCULATED,
            Sets.asSet(SourceType.CALCULATED))
        .build();

    setConsumptionsInTemplate(template);
    mockResponses(template);

    return template;
  }

  private RequisitionTemplate mockMessageAndGetRequisitionTemplate(String messageKey) {
    when(messageService.localize(new Message(messageKey))).thenReturn(message);

    RequisitionTemplate requisitionTemplate =
        getRequisitionTemplateForTestAdjustedAndAverageConsumptionField();
    requisitionTemplate.setNumberOfPeriodsToAverage(2);
    return requisitionTemplate;
  }

  private RequisitionTemplate getRequisitionTemplateForTestAdjustedAndAverageConsumptionField() {
    RequisitionTemplate template = baseTemplateBuilder()
        .withColumn(TOTAL_STOCKOUT_DAYS, "X", SourceType.USER_INPUT,
            Sets.asSet(SourceType.USER_INPUT))
        .withColumn(ADJUSTED_CONSUMPTION, "N", SourceType.CALCULATED,
            Sets.asSet(SourceType.CALCULATED))
        .withColumn(AVERAGE_CONSUMPTION, "P", SourceType.CALCULATED,
            Sets.asSet(SourceType.CALCULATED))
        .build();
    mockResponses(template);

    return template;
  }

  private void mockResponses(RequisitionTemplate template) {
    template.getColumnsMap().values().stream()
        .forEach(column -> {
          when(availableRequisitionColumnRepository.findOne(column.getColumnDefinition().getId()))
              .thenReturn(column.getColumnDefinition());
        });
    when(programReferenceDataService.findOne(template.getProgramId())).thenReturn(
        new ProgramDtoDataBuilder().withId(template.getProgramId()).build());
  }

  private void setConsumptionsInTemplate(RequisitionTemplate requisitionTemplate) {
    requisitionTemplate.changeColumnSource(ADJUSTED_CONSUMPTION, SourceType.CALCULATED);
    requisitionTemplate.changeColumnSource(AVERAGE_CONSUMPTION, SourceType.CALCULATED);
    requisitionTemplate.setNumberOfPeriodsToAverage(2);
  }
}
