package org.openlmis.requisition.validate;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.contains;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.RequisitionTemplateColumn.COLUMN_DEFINITION;
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
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_COLUMN_DEFINITION_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_FIELD_IS_TOO_LONG;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALIDATION_FIELD_MUST_BE_IN_TEMPLATE;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.ADJUSTED_CONSUMPTION;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.AVERAGE_CONSUMPTION;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.COLUMNS_MAP;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.MAX_COLUMN_DEFINITION_LENGTH;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.NUMBER_OF_PERIODS_TO_AVERAGE;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.REQUESTED_QUANTITY;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.REQUESTED_QUANTITY_EXPLANATION;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.STOCK_ON_HAND;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.TOTAL_CONSUMED_QUANTITY;
import static org.openlmis.requisition.validate.RequisitionTemplateValidator.TOTAL_STOCKOUT_DAYS;

import org.apache.commons.lang.RandomStringUtils;
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
import org.openlmis.requisition.domain.SourceType;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.repository.AvailableRequisitionColumnRepository;
import org.openlmis.utils.Message;
import org.springframework.validation.Errors;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
@RunWith(MockitoJUnitRunner.class)
public class RequisitionTemplateValidatorTest {

  private static final String COLUMN_NAME = "test";
  private static final String MUST_BE_DISPLAYED_WHEN_ADJUSTED_CONSUMPTION_IS_CALCULATED =
      " must be displayed when adjusted consumption is calculated.";

  private static final List<String> CALCULATED_ONLY = Arrays.asList("total", "packsToShip",
      "totalCost", "adjustedConsumption", "averageConsumption", "maximumStockQuantity",
      "calculatedOrderQuantity");

  @Mock
  private MessageService messageService;

  @Mock
  private AvailableRequisitionColumnRepository availableRequisitionColumnRepository;

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
    Message message8 = new Message(ERROR_MUST_BE_DISPLAYED_WHEN_ON_HAND_IS_CALCULATED,
        TOTAL_CONSUMED_QUANTITY);

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
  public void shouldRejectWhenTotalStockoutDaysFieldIsNotDisplayed() {
    RequisitionTemplate requisitionTemplate =
        getRequisitionTemplateForTestAdjustedAndAverageConsumptionField();
    requisitionTemplate.changeColumnDisplay(
        TOTAL_STOCKOUT_DAYS, false);
    requisitionTemplate.setNumberOfPeriodsToAverage(2);

    Message message6 = new Message(ERROR_MUST_BE_DISPLAYED_WHEN_CONSUMPTION_IS_CALCULATED,
        TOTAL_STOCKOUT_DAYS);
    when(messageService.localize(message6)).thenReturn(message6.new LocalizedMessage(
        TOTAL_STOCKOUT_DAYS
            + MUST_BE_DISPLAYED_WHEN_ADJUSTED_CONSUMPTION_IS_CALCULATED));

    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP),
        contains(TOTAL_STOCKOUT_DAYS
            + MUST_BE_DISPLAYED_WHEN_ADJUSTED_CONSUMPTION_IS_CALCULATED));
  }

  @Test
  public void shouldRejectWhenAverageInTemplateAndAdjustedConsumptionNotInTemplate() {
    when(messageService.localize(
        new Message(ERROR_VALIDATION_FIELD_MUST_BE_IN_TEMPLATE)))
        .thenReturn(message);

    Map<String, RequisitionTemplateColumn> columnMap = getRequisitionTemplateColumnMap();
    columnMap.put(AVERAGE_CONSUMPTION,
        generateTemplateColumn(AVERAGE_CONSUMPTION, "P"));

    RequisitionTemplate requisitionTemplate = new RequisitionTemplate(columnMap);
    requisitionTemplate.setNumberOfPeriodsToAverage(2);
    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(COLUMNS_MAP, message.toString());
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

    Map<String, RequisitionTemplateColumn> columnMap = getRequisitionTemplateColumnMap();

    RequisitionTemplate requisitionTemplate = new RequisitionTemplate(columnMap);
    requisitionTemplate.setNumberOfPeriodsToAverage(1);

    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(NUMBER_OF_PERIODS_TO_AVERAGE, message.toString());
  }

  @Test
  public void shouldNotRejectWhenNumberOfPreviousPeriodsGreaterOrEqualThanTwo() {
    Map<String, RequisitionTemplateColumn> columnMap = getRequisitionTemplateColumnMap();

    RequisitionTemplate requisitionTemplate = new RequisitionTemplate(columnMap);
    requisitionTemplate.setNumberOfPeriodsToAverage(2);

    validator.validate(requisitionTemplate, errors);

    verify(errors, never()).rejectValue(eq(NUMBER_OF_PERIODS_TO_AVERAGE), any());
  }

  @Test
  public void shouldRejectIfColumnIsNotDisplayedHasUserInputSourceAndSeveralAvailableSources() {
    Message message = new Message(ERROR_MUST_BE_DISPLAYED, STOCK_ON_HAND);
    String errorMessage = "stockOnHand must be displayed";

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        errorMessage
    ));

    Map<String, RequisitionTemplateColumn> columnMap = getRequisitionTemplateColumnMap();
    columnMap.get(STOCK_ON_HAND).setIsDisplayed(false);
    columnMap.get(STOCK_ON_HAND).setSource(SourceType.USER_INPUT);

    RequisitionTemplate requisitionTemplate = new RequisitionTemplate(columnMap);
    requisitionTemplate.setNumberOfPeriodsToAverage(2);

    validator.validate(requisitionTemplate, errors);

    verify(errors, never()).rejectValue(COLUMNS_MAP, errorMessage);
  }

  @Test
  public void shouldRejectIfColumnDefinitionIsTooLong() throws Exception {
    Message message = new Message(ERROR_VALIDATION_FIELD_IS_TOO_LONG,
        DEFINITION, MAX_COLUMN_DEFINITION_LENGTH);
    String errorMessage = DEFINITION
        + " is too long. The maximum length is "
        + MAX_COLUMN_DEFINITION_LENGTH;

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    Map<String, RequisitionTemplateColumn> columnMap = getRequisitionTemplateColumnMap();
    columnMap.get(STOCK_ON_HAND).setDefinition(RandomStringUtils.random(200));

    RequisitionTemplate requisitionTemplate = new RequisitionTemplate(columnMap);

    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfColumnDefinitionCannotBeFound() throws Exception {
    Message message = new Message(ERROR_VALIDATION_COLUMN_DEFINITION_NOT_FOUND,
        COLUMN_DEFINITION, "mocked");
    String errorMessage = "Cannot find column definition for column mocked";

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    Map<String, RequisitionTemplateColumn> columnMap = getRequisitionTemplateColumnMap();
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate(columnMap);

    when(availableRequisitionColumnRepository.findOne(columnMap.get(STOCK_ON_HAND)
        .getColumnDefinition().getId())).thenReturn(null);

    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  @Test
  public void shouldRejectIfColumnDefinitionWasModified() throws Exception {
    Message message = new Message(ERROR_VALIDATION_COLUMN_DEFINITION_NOT_FOUND,
        COLUMN_DEFINITION, "mocked");
    String errorMessage = "Cannot find column definition for column mocked";

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(errorMessage));

    Map<String, RequisitionTemplateColumn> columnMap = getRequisitionTemplateColumnMap();
    columnMap.get(STOCK_ON_HAND).setColumnDefinition(new AvailableRequisitionColumn());
    
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate(columnMap);

    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(eq(COLUMNS_MAP), contains(errorMessage));
  }

  private RequisitionTemplate generateTemplate() {
    Map<String, RequisitionTemplateColumn> columnMap = getRequisitionTemplateColumnMap();
    RequisitionTemplateColumn column = generateTemplateColumn(COLUMN_NAME, "T");

    columnMap.put(COLUMN_NAME, column);

    return new RequisitionTemplate(columnMap);
  }

  private Map<String, RequisitionTemplateColumn> getRequisitionTemplateColumnMap() {
    Map<String, RequisitionTemplateColumn> columnMap = new HashMap<>();
    columnMap.put(REQUESTED_QUANTITY, generateTemplateColumn(REQUESTED_QUANTITY, "J"));
    columnMap.put(REQUESTED_QUANTITY_EXPLANATION,
        generateTemplateColumn(REQUESTED_QUANTITY_EXPLANATION, "W"));
    columnMap.put(TOTAL_CONSUMED_QUANTITY, generateTemplateColumn(TOTAL_CONSUMED_QUANTITY, "C"));
    columnMap.put(STOCK_ON_HAND, generateTemplateColumn(STOCK_ON_HAND, "E"));

    return columnMap;
  }

  private RequisitionTemplate getRequisitionTemplateForTestAdjustedAndAverageConsumptionField() {
    Map<String, RequisitionTemplateColumn> columnMap = getRequisitionTemplateColumnMap();
    columnMap.put(TOTAL_STOCKOUT_DAYS, generateTemplateColumn(TOTAL_STOCKOUT_DAYS, "X"));
    columnMap.put(ADJUSTED_CONSUMPTION, generateTemplateColumn(ADJUSTED_CONSUMPTION, "N"));
    columnMap.put(AVERAGE_CONSUMPTION, generateTemplateColumn(AVERAGE_CONSUMPTION, "P"));

    RequisitionTemplate requisitionTemplate = new RequisitionTemplate(columnMap);
    requisitionTemplate.changeColumnSource(ADJUSTED_CONSUMPTION, SourceType.CALCULATED);
    requisitionTemplate.changeColumnSource(AVERAGE_CONSUMPTION, SourceType.CALCULATED);
    requisitionTemplate.changeColumnSource(TOTAL_STOCKOUT_DAYS, SourceType.USER_INPUT);
    requisitionTemplate.changeColumnSource(TOTAL_CONSUMED_QUANTITY, SourceType.USER_INPUT);

    return requisitionTemplate;
  }

  private RequisitionTemplateColumn generateTemplateColumn(String name, String indicator) {
    AvailableRequisitionColumn columnDefinition = new AvailableRequisitionColumn();
    columnDefinition.setId(UUID.randomUUID());
    columnDefinition.setName(name);
    columnDefinition.setIndicator(indicator);
    columnDefinition.setIsDisplayRequired(false);
    Set<SourceType> sources = new HashSet<>();

    if (!CALCULATED_ONLY.contains(name)) {
      sources.add(SourceType.USER_INPUT);
    }

    if (CALCULATED_ONLY.contains(name)
        || STOCK_ON_HAND.equalsIgnoreCase(name)
        || TOTAL_CONSUMED_QUANTITY.equalsIgnoreCase(name)) {
      sources.add(SourceType.CALCULATED);
    }

    columnDefinition.setSources(sources);

    when(availableRequisitionColumnRepository.findOne(columnDefinition.getId()))
        .thenReturn(columnDefinition);

    RequisitionTemplateColumn requisitionTemplateColumn =
        new RequisitionTemplateColumn(columnDefinition);
    requisitionTemplateColumn.setName(name);
    requisitionTemplateColumn.setIsDisplayed(true);

    return requisitionTemplateColumn;
  }
}
