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
import static org.openlmis.requisition.domain.SourceType.CALCULATED;
import static org.openlmis.requisition.domain.SourceType.STOCK_CARDS;
import static org.openlmis.requisition.domain.SourceType.USER_INPUT;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_CANNOT_CALCULATE_AT_THE_SAME_TIME;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_COLUMN_SOURCE_INVALID;
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
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SOURCE_NOT_AVAILABLE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_SOURCE_OF_REQUISITION_TEMPLATE_COLUMN_CANNOT_BE_NULL;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_COLUMN_DEFINITION_MODIFIED;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_COLUMN_DEFINITION_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_FIELD_IS_TOO_LONG;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_FIELD_MUST_BE_IN_TEMPLATE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_REFERENCED_OBJECT_DOES_NOT_EXIST;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.ADJUSTED_CONSUMPTION;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.AVERAGE_CONSUMPTION;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.BEGINNING_BALANCE;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.CALCULATED_ORDER_QUANTITY;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.CALCULATED_ORDER_QUANTITY_ISA;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.COLUMNS_MAP;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.FACILITY_TYPE;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.FACILITY_TYPE_ID;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.MAX_COLUMN_DEFINITION_LENGTH;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.NUMBER_OF_PERIODS_TO_AVERAGE;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.PROGRAM;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.PROGRAM_ID;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.REQUESTED_QUANTITY;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.REQUESTED_QUANTITY_EXPLANATION;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.STOCK_BASED_COLUMNS;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.STOCK_DISABLED_COLUMNS;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.STOCK_ON_HAND;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.TOTAL_CONSUMED_QUANTITY;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.TOTAL_LOSSES_AND_ADJUSTMNETS;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.TOTAL_RECEIVED_QUANTITY;
import static org.openlmis.requisition.validate.RequisitionTemplateDtoValidator.TOTAL_STOCKOUT_DAYS;

import com.google.common.collect.ImmutableSet;

import org.apache.commons.lang.RandomStringUtils;
import org.javers.common.collections.Sets;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.AvailableRequisitionColumnOption;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.dto.AvailableRequisitionColumnOptionDto;
import org.openlmis.requisition.dto.FacilityTypeDto;
import org.openlmis.requisition.dto.RequisitionTemplateColumnDto;
import org.openlmis.requisition.dto.RequisitionTemplateDto;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.repository.AvailableRequisitionColumnRepository;
import org.openlmis.requisition.service.referencedata.FacilityTypeReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.testutils.AvailableRequisitionColumnDataBuilder;
import org.openlmis.requisition.testutils.AvailableRequisitionColumnOptionDataBuilder;
import org.openlmis.requisition.testutils.ProgramDtoDataBuilder;
import org.openlmis.requisition.domain.RequisitionTemplateColumnDataBuilder;
import org.openlmis.requisition.domain.RequisitionTemplateDataBuilder;
import org.openlmis.requisition.utils.Message;
import org.springframework.validation.Errors;

import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
@RunWith(MockitoJUnitRunner.class)
public class RequisitionTemplateDtoValidatorTest {

  private static final String COLUMN_NAME = "test";
  private static final String ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED =
      "only alphanumeric label is accepted";

  @Mock
  private MessageService messageService;

  @Mock
  private AvailableRequisitionColumnRepository availableRequisitionColumnRepository;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private FacilityTypeReferenceDataService facilityTypeReferenceDataService;

  @InjectMocks
  private RequisitionTemplateDtoValidator validator;

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
    RequisitionTemplateDto requisitionTemplate = generateTemplate();
    requisitionTemplate.getColumnsMap().get(REQUESTED_QUANTITY_EXPLANATION).setIsDisplayed(false);
    requisitionTemplate.getColumnsMap().get(REQUESTED_QUANTITY).setSource(USER_INPUT);
    requisitionTemplate.getColumnsMap().get(REQUESTED_QUANTITY_EXPLANATION).setSource(USER_INPUT);

    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP),
        contains(REQUESTED_QUANTITY_EXPLANATION
            + " must be displayed when requested quantity is displayed."));
  }

  @Test
  public void shouldRejectWhenRequestedQuantityAndCalcOrderQuantityAreNotDisplayed() {
    RequisitionTemplateDto requisitionTemplate = generateTemplate();
    requisitionTemplate.getColumnsMap().get(REQUESTED_QUANTITY).setIsDisplayed(false);
    requisitionTemplate.getColumnsMap().get(REQUESTED_QUANTITY_EXPLANATION).setIsDisplayed(false);
    requisitionTemplate.getColumnsMap().get(CALCULATED_ORDER_QUANTITY).setIsDisplayed(false);

    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP),
        contains(REQUESTED_QUANTITY
            + " must be displayed when calculated order quantity is not displayed."));
  }

  @Test
  public void shouldRejectWhenSourceIsNotAvailableInColumn() {
    RequisitionTemplateDto requisitionTemplate = generateTemplate();

    requisitionTemplate.getColumnsMap().get(COLUMN_NAME).setSource(USER_INPUT);
    requisitionTemplate.getColumnsMap()
        .get(COLUMN_NAME).getColumnDefinition().getSources().clear();

    Message message4 = new Message(ERROR_SOURCE_NOT_AVAILABLE, "test");
    when(messageService.localize(message4)).thenReturn(message4.new LocalizedMessage(
        "Source " + USER_INPUT + " is not available for this column."));

    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP),
        contains(RequisitionTemplate.SOURCE + USER_INPUT
            + RequisitionTemplate.WARNING_SUFFIX));
  }

  @Test
  public void shouldRejectWhenOptionIsNotAvailableInColumn() {
    RequisitionTemplateDto requisitionTemplate = generateTemplate();
    RequisitionTemplateColumnDto column = requisitionTemplate
        .getColumnsMap()
        .get(COLUMN_NAME);

    AvailableRequisitionColumnOption option = new AvailableRequisitionColumnOptionDataBuilder()
        .build();

    column.setOption(AvailableRequisitionColumnOptionDto.newInstance(option));

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
    RequisitionTemplateDto template = addAdjustedConsumptionToColumnsMapAndGetRequisitionTemplate();
    RequisitionTemplateColumn column = new RequisitionTemplateColumnDataBuilder()
        .withName(TOTAL_STOCKOUT_DAYS)
        .withIndicator("X")
        .withColumnDefinition(new AvailableRequisitionColumnDataBuilder()
            .withName(TOTAL_STOCKOUT_DAYS)
            .withSources(Sets.asSet(USER_INPUT))
            .withoutOptions()
            .build())
        .withSource(USER_INPUT)
        .withoutOption()
        .build();
    template
        .getColumnsMap()
        .put(TOTAL_STOCKOUT_DAYS, RequisitionTemplateColumnDto.newInstance(column));

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

    RequisitionTemplateDto template = addAdjustedConsumptionToColumnsMapAndGetRequisitionTemplate();

    validator.validate(template, errors);

    verify(errors, times(2)).rejectValue(COLUMNS_MAP, message.toString());
  }

  @Test
  public void shouldNotRejectWhenConsumptionsAndConsumedQuantityAreInTemplate() {
    RequisitionTemplateDto template = addAdjustedConsumptionToColumnsMapAndGetRequisitionTemplate();
    RequisitionTemplateColumn column = new RequisitionTemplateColumnDataBuilder()
        .withName(TOTAL_STOCKOUT_DAYS)
        .withIndicator("X")
        .withColumnDefinition(new AvailableRequisitionColumnDataBuilder()
            .withName(TOTAL_STOCKOUT_DAYS)
            .withSources(Sets.asSet(USER_INPUT))
            .withoutOptions()
            .build())
        .withSource(USER_INPUT)
        .withoutOption()
        .build();
    template
        .getColumnsMap()
        .put(TOTAL_STOCKOUT_DAYS, RequisitionTemplateColumnDto.newInstance(column));

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

    RequisitionTemplateDto template = addAdjustedConsumptionToColumnsMapAndGetRequisitionTemplate();

    validator.validate(template, errors);

    verify(errors, times(2)).rejectValue(COLUMNS_MAP, message.toString());
  }

  @Test
  public void shouldRejectWhenAdjustedConsumptionIsDisplayedAndStockoutDaysIsNotDisplayed() {
    RequisitionTemplateDto requisitionTemplate = mockMessageAndGetRequisitionTemplate(
        ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED);
    requisitionTemplate.getColumnsMap().get(TOTAL_STOCKOUT_DAYS).setIsDisplayed(false);
    requisitionTemplate.getColumnsMap().get(AVERAGE_CONSUMPTION).setIsDisplayed(false);
    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(COLUMNS_MAP, message.toString());
  }

  @Test
  public void shouldRejectWhenAverageConsumptionIsDisplayedAndStockoutDaysIsNotDisplayed() {
    RequisitionTemplateDto requisitionTemplate = mockMessageAndGetRequisitionTemplate(
        ERROR_MUST_BE_DISPLAYED_WHEN_AVERAGE_CONSUMPTION_IS_CALCULATED);
    requisitionTemplate.getColumnsMap().get(TOTAL_STOCKOUT_DAYS).setIsDisplayed(false);
    requisitionTemplate.getColumnsMap().get(ADJUSTED_CONSUMPTION).setIsDisplayed(false);
    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(COLUMNS_MAP, message.toString());
  }

  @Test
  public void shouldNotRejectWhenAverageConsumptionIsDisplayedAndStockoutDaysIsDisabled() {
    RequisitionTemplateDto requisitionTemplate = getTemplatePopulatedByStock();
    RequisitionTemplateColumn column = new RequisitionTemplateColumnDataBuilder()
        .withName(TOTAL_STOCKOUT_DAYS)
        .withIndicator("X")
        .withNotDisplayed()
        .withColumnDefinition(new AvailableRequisitionColumnDataBuilder()
            .withName(TOTAL_STOCKOUT_DAYS)
            .withSources(Sets.asSet(USER_INPUT))
            .build())
        .withSource(USER_INPUT)
        .build();
    requisitionTemplate
        .getColumnsMap()
        .put(TOTAL_STOCKOUT_DAYS, RequisitionTemplateColumnDto.newInstance(column));
    validator.validate(requisitionTemplate, errors);

    verify(errors, never()).rejectValue(anyString(), anyString());
  }

  @Test
  public void shouldNotRejectWhenConsumptionsAreDisplayedAndStockoutDaysIsDisplayed() {
    RequisitionTemplateDto requisitionTemplate = mockMessageAndGetRequisitionTemplate(
        ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED);
    requisitionTemplate.getColumnsMap().get(TOTAL_STOCKOUT_DAYS).setIsDisplayed(true);
    validator.validate(requisitionTemplate, errors);

    verify(errors, never()).rejectValue(anyString(), anyString());
  }

  @Test
  public void shouldNotRejectWhenConsumptionsAreNotDisplayedAndStockoutDaysIsNotDisplayed() {
    RequisitionTemplateDto requisitionTemplate = mockMessageAndGetRequisitionTemplate(
        ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED);
    requisitionTemplate.getColumnsMap().get(AVERAGE_CONSUMPTION).setIsDisplayed(false);
    requisitionTemplate.getColumnsMap().get(ADJUSTED_CONSUMPTION).setIsDisplayed(false);
    requisitionTemplate.getColumnsMap().get(TOTAL_STOCKOUT_DAYS).setIsDisplayed(false);
    validator.validate(requisitionTemplate, errors);

    verify(errors, never()).rejectValue(anyString(), anyString());
  }

  @Test
  public void shouldNotRejectWhenConsumptionsAreDisplayedAndConsumedQuantityIsCalculated() {
    RequisitionTemplateDto requisitionTemplate = mockMessageAndGetRequisitionTemplate(
        ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED);
    requisitionTemplate.getColumnsMap().get(TOTAL_CONSUMED_QUANTITY).setSource(CALCULATED);
    requisitionTemplate.getColumnsMap().get(TOTAL_CONSUMED_QUANTITY).setIsDisplayed(false);
    validator.validate(requisitionTemplate, errors);

    verify(errors, never()).rejectValue(anyString(), anyString());
  }

  @Test
  public void shouldNotRejectWhenConsumptionsAreDisplayedAndConsumedQuantityIsDisplayed() {
    RequisitionTemplateDto requisitionTemplate = mockMessageAndGetRequisitionTemplate(
        ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED);
    requisitionTemplate.getColumnsMap().get(TOTAL_CONSUMED_QUANTITY).setIsDisplayed(true);
    validator.validate(requisitionTemplate, errors);

    verify(errors, never()).rejectValue(anyString(), anyString());
  }

  @Test
  public void shouldNotRejectWhenConsumptionsAndConsumedQuantityAreNotDisplayed() {
    RequisitionTemplateDto requisitionTemplate = mockMessageAndGetRequisitionTemplate(
        ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED);
    requisitionTemplate.getColumnsMap().get(TOTAL_CONSUMED_QUANTITY).setSource(CALCULATED);
    requisitionTemplate.getColumnsMap().get(AVERAGE_CONSUMPTION).setIsDisplayed(false);
    requisitionTemplate.getColumnsMap().get(ADJUSTED_CONSUMPTION).setIsDisplayed(false);
    requisitionTemplate.getColumnsMap().get(TOTAL_CONSUMED_QUANTITY).setIsDisplayed(false);
    validator.validate(requisitionTemplate, errors);

    verify(errors, never()).rejectValue(anyString(), anyString());
  }

  @Test
  public void shouldRejectWhenAverageInTemplateAndNumberOfPreviousPeriodsIsNull() {
    when(messageService.localize(
        new Message("requisition.error.validation.fieldCanNotBeNull")))
        .thenReturn(message);

    RequisitionTemplateDto requisitionTemplate
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
    RequisitionTemplateDto dto = buildDto(template);
    mockResponses(dto);

    validator.validate(dto, errors);

    verify(errors).rejectValue(NUMBER_OF_PERIODS_TO_AVERAGE, message.toString());
  }

  @Test
  public void shouldNotRejectWhenNumberOfPreviousPeriodsGreaterOrEqualThanTwo() {
    RequisitionTemplate template = baseTemplateBuilder()
        .withNumberOfPeriodsToAverage(2)
        .build();

    RequisitionTemplateDto dto = buildDto(template);
    mockResponses(dto);

    validator.validate(dto, errors);

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

    RequisitionTemplateDto dto = buildDto(template);
    dto.getColumnsMap().get(STOCK_ON_HAND).setIsDisplayed(false);
    dto.getColumnsMap().get(STOCK_ON_HAND).setSource(USER_INPUT);

    mockResponses(dto);

    validator.validate(dto, errors);

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
    RequisitionTemplateDto dto = buildDto(template);
    dto.getColumnsMap().get(STOCK_ON_HAND).setDefinition(RandomStringUtils.random(200));
    mockResponses(dto);

    validator.validate(dto, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfColumnDefinitionCannotBeFound() throws Exception {
    Message message = new Message(ERROR_VALIDATION_COLUMN_DEFINITION_NOT_FOUND,
        COLUMN_DEFINITION, "mocked");
    String errorMessage = "Cannot find column definition for column mocked";

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    RequisitionTemplateDto template = generateTemplate();

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

    RequisitionTemplateDto template = generateTemplate();

    when(availableRequisitionColumnRepository
        .findOne(template.getColumnsMap().get(STOCK_ON_HAND).getColumnDefinition().getId()))
        .thenReturn(new AvailableRequisitionColumn());

    validator.validate(template, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfColumnLabelIsInvalid() {
    Message message = new Message(ERROR_ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED);
    String errorMessage = ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED;

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    RequisitionTemplateDto template = generateTemplate();

    template.getColumnsMap().get(STOCK_ON_HAND)
        .setLabel("New not valid name with wrong signs: !@#$%^&*()");
    validator.validate(template, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfColumnLabelNameHasSpecialCharacters() {
    Message message = new Message(ERROR_ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED);
    String errorMessage = ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED;

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    RequisitionTemplateDto template = generateTemplate();

    template.getColumnsMap().get(STOCK_ON_HAND).setLabel(")(*&^%$#@!");
    validator.validate(template, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfColumnLabelIsNull() {
    Message message = new Message(ERROR_ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED);
    String errorMessage = ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED;

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    RequisitionTemplateDto template = generateTemplate();

    template.getColumnsMap().get(STOCK_ON_HAND).setLabel(null);
    validator.validate(template, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfColumnLabelIsEmpty() {
    Message message = new Message(ERROR_ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED);
    String errorMessage = ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED;

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    RequisitionTemplateDto template = generateTemplate();

    template.getColumnsMap().get(STOCK_ON_HAND).setLabel("");
    validator.validate(template, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfColumnLabelNameHasOnlyWhiteSpace() {
    Message message = new Message(ERROR_ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED);
    String errorMessage = ONLY_ALPHANUMERIC_LABEL_IS_ACCEPTED;

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    RequisitionTemplateDto template = generateTemplate();

    template.getColumnsMap().get(STOCK_ON_HAND).setLabel(" ");
    validator.validate(template, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfProgramWithSpecifiedIdDoesNotExist() throws Exception {

    RequisitionTemplateDto requisitionTemplate = generateTemplate();
    when(programReferenceDataService.findOne(requisitionTemplate.getProgramId())).thenReturn(null);

    Message message = new Message(ERROR_VALIDATION_REFERENCED_OBJECT_DOES_NOT_EXIST,
        PROGRAM, requisitionTemplate.getProgramId());
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        message.toString()));

    validator.validate(requisitionTemplate, errors);
    verify(errors).rejectValue(eq(PROGRAM_ID), contains(message.toString()));
  }

  @Test
  public void shouldRejectIfFacilityTypeWithSpecifiedIdDoesNotExist() throws Exception {
    RequisitionTemplateDto requisitionTemplate = generateTemplate();
    UUID facilityTypeId = requisitionTemplate.getFacilityTypeIds().iterator().next();
    when(facilityTypeReferenceDataService.findOne(facilityTypeId)).thenReturn(null);

    Message message = new Message(ERROR_VALIDATION_REFERENCED_OBJECT_DOES_NOT_EXIST,
        FACILITY_TYPE, facilityTypeId);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        message.toString()));

    validator.validate(requisitionTemplate, errors);
    verify(errors).rejectValue(eq(FACILITY_TYPE_ID), contains(message.toString()));
  }

  @Test
  public void shouldRejectWhenSourceInRequisitionTemplateColumnIsNull() throws Exception {
    RequisitionTemplateDto requisitionTemplate = generateTemplate();
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
    RequisitionTemplateDto template = getTemplatePopulatedByStock();

    validator.validate(template, errors);

    Message message = new Message(ERROR_COLUMN_SOURCE_INVALID);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        message.toString()));

    template.findColumn(STOCK_ON_HAND).setSource(CALCULATED);

    validator.validate(template, errors);
    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(message.toString()));
  }

  @Test
  public void shouldRejectWhenTotalReceivedQuantitySourceIsOtherThanStockCards() {
    RequisitionTemplateDto template = getTemplatePopulatedByStock();

    validator.validate(template, errors);

    Message message = new Message(ERROR_COLUMN_SOURCE_INVALID);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        message.toString()));

    template.findColumn(TOTAL_RECEIVED_QUANTITY).setSource(CALCULATED);

    validator.validate(template, errors);
    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(message.toString()));
  }

  @Test
  public void shouldRejectWhenTotalConsumedQuantitySourceIsOtherThanStockCards() {
    RequisitionTemplateDto template = getTemplatePopulatedByStock();

    validator.validate(template, errors);

    Message message = new Message(ERROR_COLUMN_SOURCE_INVALID);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        message.toString()));

    template.findColumn(TOTAL_CONSUMED_QUANTITY).setSource(CALCULATED);

    validator.validate(template, errors);
    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(message.toString()));
  }

  @Test
  public void shouldNotRejectWhenTotalConsumedQuantityIsDisplayedForStockBasedRequisition() {
    RequisitionTemplateDto template = getTemplatePopulatedByStock();

    validator.validate(template, errors);

    template.findColumn(TOTAL_CONSUMED_QUANTITY).setIsDisplayed(true);

    verify(errors, never()).rejectValue(anyString(), anyString());
  }

  @Test
  public void shouldNotRejectWhenTotalLossesAndAdjustmentIsDisplayedForStockBasedRequisition() {
    RequisitionTemplateDto template = getTemplatePopulatedByStock();

    validator.validate(template, errors);

    template.findColumn(TOTAL_LOSSES_AND_ADJUSTMNETS).setIsDisplayed(true);

    verify(errors, never()).rejectValue(anyString(), anyString());
  }

  @Test
  public void shouldRejectWhenCalcOrderQtyIsaIsDisplayedAndStockFlagIsDisabled() {
    when(messageService
        .localize(new Message(ERROR_MUST_NOT_BE_DISPLAYED_WHEN_SOH_POPULATED_FROM_STOCK_CARDS)))
        .thenReturn(message);
    RequisitionTemplate template = baseTemplateBuilder()
        .withColumn(CALCULATED_ORDER_QUANTITY_ISA, "S", CALCULATED, ImmutableSet.of(CALCULATED))
        .build();

    RequisitionTemplateDto dto = buildDto(template);
    mockResponses(dto);

    dto.getColumnsMap().get(CALCULATED_ORDER_QUANTITY_ISA).setIsDisplayed(true);

    validator.validate(dto, errors);

    verify(errors).rejectValue(COLUMNS_MAP, message.toString());
  }

  private RequisitionTemplateDataBuilder baseTemplateBuilder() {
    return new RequisitionTemplateDataBuilder()
        .withRequiredColumns()
        .withColumn(TOTAL_CONSUMED_QUANTITY, "C", USER_INPUT,
            Sets.asSet(USER_INPUT, CALCULATED))
        .withColumn(STOCK_ON_HAND, "E", USER_INPUT,
            Sets.asSet(USER_INPUT, CALCULATED))
        .withColumn(BEGINNING_BALANCE, "A", USER_INPUT,
            Sets.asSet(USER_INPUT))
        .withColumn(COLUMN_NAME, "T", USER_INPUT,
            Sets.asSet(USER_INPUT))
        .withAssignment(UUID.randomUUID(), UUID.randomUUID());
  }

  private RequisitionTemplateDto generateTemplate() {
    RequisitionTemplate template = baseTemplateBuilder().build();
    RequisitionTemplateDto dto = buildDto(template);
    mockResponses(dto);
    return dto;
  }

  private RequisitionTemplateDto getTemplatePopulatedByStock() {
    RequisitionTemplate template = baseTemplateBuilder()
        .withColumn(TOTAL_RECEIVED_QUANTITY, "B", USER_INPUT,
            Sets.asSet(USER_INPUT))
        .withColumn(TOTAL_LOSSES_AND_ADJUSTMNETS, "D", STOCK_CARDS,
            Sets.asSet(STOCK_CARDS))
        .withPopulateStockOnHandFromStockCards()
        .build();

    RequisitionTemplateDto dto = buildDto(template);

    dto.findColumn(TOTAL_CONSUMED_QUANTITY).setTag("consumed");
    dto.findColumn(TOTAL_RECEIVED_QUANTITY).setTag("received");
    dto.findColumn(TOTAL_LOSSES_AND_ADJUSTMNETS).setTag("adjustment");

    STOCK_DISABLED_COLUMNS
        .stream()
        .filter(dto::isColumnInTemplate)
        .forEach(c -> dto.getColumnsMap().get(c).setIsDisplayed(false));

    STOCK_BASED_COLUMNS
        .stream()
        .filter(dto::isColumnInTemplate)
        .forEach(c -> {
          dto.findColumn(c).setSource(STOCK_CARDS);
          dto.findColumn(c).getColumnDefinition().getSources().add(STOCK_CARDS);
        });

    mockResponses(dto);

    return dto;
  }

  private RequisitionTemplateDto addAdjustedConsumptionToColumnsMapAndGetRequisitionTemplate() {
    RequisitionTemplate template = baseTemplateBuilder()
        .withColumn(TOTAL_CONSUMED_QUANTITY, "X", USER_INPUT,
            Sets.asSet(USER_INPUT))
        .withColumn(ADJUSTED_CONSUMPTION, "N", CALCULATED,
            Sets.asSet(CALCULATED))
        .withColumn(AVERAGE_CONSUMPTION, "P", CALCULATED,
            Sets.asSet(CALCULATED))
        .build();


    RequisitionTemplateDto dto = buildDto(template);
    mockResponses(dto);
    setConsumptionsInTemplate(dto);

    return dto;
  }

  private RequisitionTemplateDto mockMessageAndGetRequisitionTemplate(String messageKey) {
    when(messageService.localize(new Message(messageKey))).thenReturn(message);

    RequisitionTemplateDto requisitionTemplate =
        getRequisitionTemplateForTestAdjustedAndAverageConsumptionField();
    requisitionTemplate.setNumberOfPeriodsToAverage(2);
    return requisitionTemplate;
  }

  private RequisitionTemplateDto getRequisitionTemplateForTestAdjustedAndAverageConsumptionField() {
    RequisitionTemplate template = baseTemplateBuilder()
        .withColumn(TOTAL_STOCKOUT_DAYS, "X", USER_INPUT,
            Sets.asSet(USER_INPUT))
        .withColumn(ADJUSTED_CONSUMPTION, "N", CALCULATED,
            Sets.asSet(CALCULATED))
        .withColumn(AVERAGE_CONSUMPTION, "P", CALCULATED,
            Sets.asSet(CALCULATED))
        .build();

    RequisitionTemplateDto dto = buildDto(template);
    mockResponses(dto);

    return dto;
  }

  private RequisitionTemplateDto buildDto(RequisitionTemplate template) {
    RequisitionTemplateDto dto = new RequisitionTemplateDto();
    template.export(dto);

    dto.setColumnsMap(RequisitionTemplateColumnDto.newInstance(template.viewColumns()));
    return dto;
  }

  private void mockResponses(RequisitionTemplateDto template) {
    for (RequisitionTemplateColumnDto column : template.getColumnsMap().values()) {
      when(availableRequisitionColumnRepository.findOne(column.getColumnDefinition().getId()))
          .thenReturn(AvailableRequisitionColumn.newInstance(column.getColumnDefinition()));
    }

    when(programReferenceDataService.findOne(template.getProgramId())).thenReturn(
        new ProgramDtoDataBuilder().withId(template.getProgramId()).build());

    for (UUID facilityTypeId : template.getFacilityTypeIds()) {
      when(facilityTypeReferenceDataService.findOne(facilityTypeId)).thenReturn(
          new FacilityTypeDto());
    }
  }

  private void setConsumptionsInTemplate(RequisitionTemplateDto requisitionTemplate) {
    requisitionTemplate.getColumnsMap().get(ADJUSTED_CONSUMPTION).setSource(CALCULATED);
    requisitionTemplate.getColumnsMap().get(AVERAGE_CONSUMPTION).setSource(CALCULATED);
    requisitionTemplate.setNumberOfPeriodsToAverage(2);
  }
}
