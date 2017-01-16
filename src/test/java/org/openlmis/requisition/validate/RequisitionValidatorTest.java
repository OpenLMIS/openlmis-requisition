package org.openlmis.requisition.validate;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.contains;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.StockAdjustment;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.StockAdjustmentReasonReferenceDataService;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.Message;
import org.springframework.validation.Errors;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@SuppressWarnings("PMD.TooManyMethods")
@RunWith(MockitoJUnitRunner.class)
public class RequisitionValidatorTest {

  private static final String ENTERED_PRIOR_SUBMISSION_NOTIFICATION =
      " must be entered prior to submission of a requisition.";
  private static final String REASON_MUST_BE_NON_NEGATIVE =
      "reasonId with id must be a non-negative value.";

  @Mock
  private MessageService messageService;
  @Mock
  private RequisitionRepository requisitionRepository;

  @Mock
  private ConfigurationSettingService configurationSettingService;

  @Mock
  private StockAdjustmentReasonReferenceDataService stockAdjustmentReasonReferenceDataService;

  @InjectMocks
  private RequisitionValidator requisitionValidator;

  private Requisition requisition = mock(Requisition.class);
  private Errors errors = mock(Errors.class);

  private List<RequisitionLineItem> requisitionLineItems;
  private RequisitionTemplate requisitionTemplate;
  private Map<String, RequisitionTemplateColumn> columnsMap;
  private UUID programId;

  @Before
  public void setUp() {
    requisitionLineItems = new ArrayList<>();
    columnsMap = RequisitionValidationTestUtils.initiateColumns();
    requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setId(UUID.randomUUID());
    requisitionTemplate.setColumnsMap(columnsMap);
    mockRepositoriesAndObjects();
  }

  @Test
  public void shouldRejectIfRequisitionItemsEmpty() {
    requisitionLineItems = new ArrayList<>();

    Message message1 = new Message(RequisitionValidator.VALUE_MUST_BE_ENTERED_NOTIFICATION,
        RequisitionValidator.REQUISITION_LINE_ITEMS);
    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        RequisitionValidator.REQUISITION_LINE_ITEMS
            + ENTERED_PRIOR_SUBMISSION_NOTIFICATION));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains("requisitionLineItems must be entered prior to submission of a requisition." ));
  }

  @Test
  public void shouldRejectIfValueIsLessThanZero() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(-1);
    requisitionLineItems.add(lineItem);

    Message message1 = new Message(RequisitionValidator.VALUE_MUST_BE_ENTERED_NOTIFICATION,
        RequisitionValidator.REQUISITION_LINE_ITEMS);
    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        RequisitionValidator.REQUISITION_LINE_ITEMS
            + ENTERED_PRIOR_SUBMISSION_NOTIFICATION));

    Message message = new Message("requisition.error.validation.must-be-non-negative",
        RequisitionLineItem.REQUESTED_QUANTITY);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        RequisitionLineItem.REQUESTED_QUANTITY
            + " must be a non-negative value."));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(RequisitionLineItem.REQUESTED_QUANTITY
            + " must be a non-negative value."));
  }

  @Test
  public void shouldRejectIfValueIsNull() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(null);
    requisitionLineItems.add(lineItem);

    Message message = new Message(RequisitionValidator.VALUE_MUST_BE_ENTERED_NOTIFICATION,
        RequisitionLineItem.REQUESTED_QUANTITY);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        RequisitionLineItem.REQUESTED_QUANTITY
            + ENTERED_PRIOR_SUBMISSION_NOTIFICATION));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(RequisitionLineItem.REQUESTED_QUANTITY
            + ENTERED_PRIOR_SUBMISSION_NOTIFICATION));
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenColumnDoesNotExist() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(1);
    requisitionLineItems.add(lineItem);

    columnsMap.remove(RequisitionLineItem.STOCK_ON_HAND);

    requisitionValidator.validate(requisition, errors);
  }

  @Test
  public void shouldRejectIfColumnIsHiddenAndValueNotEmpty() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setStockOnHand(1);
    requisitionLineItems.add(lineItem);

    columnsMap.get(RequisitionLineItem.STOCK_ON_HAND).setIsDisplayed(false);

    Message message = new Message("requisition.error.validation.is-hidden",
        RequisitionLineItem.STOCK_ON_HAND);
    when(messageService.localize(message)).thenReturn(
        message.new LocalizedMessage("is hidden in template and should not contain a value."));

    Message message1 = new Message(RequisitionValidator.VALUE_MUST_BE_ENTERED_NOTIFICATION,
        RequisitionValidator.REQUISITION_LINE_ITEMS);
    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        RequisitionValidator.REQUISITION_LINE_ITEMS
            + ENTERED_PRIOR_SUBMISSION_NOTIFICATION));

    requisitionValidator.validate(requisition, errors);

    // 1. when we check if value is not null
    // 2. when we check if values is greater or equal to zero
    // 3. when we check if calculation was correct
    verify(errors, times(3)).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains("is hidden in template and should not contain a value."));
  }

  @Test
  public void shouldRejectIfStockOnHandIsIncorrectlyCalculated() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setStockOnHand(2);
    requisitionLineItems.add(lineItem);

    Message message1 = new Message(RequisitionValidator.VALUE_MUST_BE_ENTERED_NOTIFICATION,
        RequisitionValidator.REQUISITION_LINE_ITEMS);
    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        RequisitionValidator.REQUISITION_LINE_ITEMS
            + ENTERED_PRIOR_SUBMISSION_NOTIFICATION));

    Message message2 = new Message("requisition.error.validation.incorrect-value",
        RequisitionLineItem.STOCK_ON_HAND,
        RequisitionLineItem.TOTAL_CONSUMED_QUANTITY);
    when(messageService.localize(message2)).thenReturn(message2.new LocalizedMessage(
        "has incorrect value, it does not match the calculated value."));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(RequisitionValidator.VALUE_IS_INCORRECTLY_CALCULATED));
  }

  @Test
  public void shouldRejectIfStockAdjustmentsHaveInvalidReasons() {
    StockAdjustment adjustment = mock(StockAdjustment.class);
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.getStockAdjustments().add(adjustment);
    requisitionLineItems.add(lineItem);

    Message message = new Message("requisition.error.validation.stock-adjustment-not-found",
        adjustment.getReasonId());
    Message message1 = new Message(RequisitionValidator.VALUE_MUST_BE_ENTERED_NOTIFICATION,
        RequisitionValidator.REQUISITION_LINE_ITEMS);
    Message message2 = new Message("requisition.error.validation.stock-adjustment-non-negative",
        adjustment.getReasonId());

    when(adjustment.getId()).thenReturn(UUID.randomUUID());
    when(messageService.localize(message)).thenReturn(
        message.new LocalizedMessage("reasonId with id  could not be found."));
    when(messageService.localize(message2)).thenReturn(
        message2.new LocalizedMessage(REASON_MUST_BE_NON_NEGATIVE));
    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        RequisitionValidator.REQUISITION_LINE_ITEMS
            + ENTERED_PRIOR_SUBMISSION_NOTIFICATION));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.STOCK_ADJUSTMENT_REASON),
        contains(RequisitionValidator.VALUE_NOT_FOUND));
  }

  @Test
  public void shouldRejectIfStockAdjustmentsHaveNullQuantity() {
    StockAdjustment adjustment = mock(StockAdjustment.class);
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.getStockAdjustments().add(adjustment);
    requisitionLineItems.add(lineItem);

    Message message1 = new Message(RequisitionValidator.VALUE_MUST_BE_ENTERED_NOTIFICATION,
        RequisitionValidator.REQUISITION_LINE_ITEMS);
    Message message = new Message("requisition.error.validation.stock-adjustment-not-found",
        adjustment.getReasonId());
    Message message2 = new Message("requisition.error.validation.stock-adjustment-non-negative",
        adjustment.getReasonId());

    when(adjustment.getQuantity()).thenReturn(null);
    when(messageService.localize(message)).thenReturn(
        message.new LocalizedMessage("must be a non-negative value."));
    when(messageService.localize(message2)).thenReturn(
        message2.new LocalizedMessage(REASON_MUST_BE_NON_NEGATIVE));
    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        RequisitionValidator.REQUISITION_LINE_ITEMS
            + ENTERED_PRIOR_SUBMISSION_NOTIFICATION));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.STOCK_ADJUSTMENT_REASON),
        contains(REASON_MUST_BE_NON_NEGATIVE));
  }

  @Test
  public void shouldRejectIfStockAdjustmentsHaveNegativeQuantity() {
    StockAdjustment adjustment = mock(StockAdjustment.class);
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.getStockAdjustments().add(adjustment);
    requisitionLineItems.add(lineItem);

    Message message = new Message("requisition.error.validation.stock-adjustment-not-found",
        adjustment.getReasonId());
    Message message1 = new Message(RequisitionValidator.VALUE_MUST_BE_ENTERED_NOTIFICATION,
        RequisitionValidator.REQUISITION_LINE_ITEMS);
    Message message2 = new Message("requisition.error.validation.stock-adjustment-non-negative",
        adjustment.getReasonId());

    when(adjustment.getQuantity()).thenReturn(-6);
    when(messageService.localize(message)).thenReturn(
        message.new LocalizedMessage("must be a non-negative value."));
    when(messageService.localize(message2)).thenReturn(
        message2.new LocalizedMessage(REASON_MUST_BE_NON_NEGATIVE));
    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        RequisitionValidator.REQUISITION_LINE_ITEMS
            + ENTERED_PRIOR_SUBMISSION_NOTIFICATION));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.STOCK_ADJUSTMENT_REASON),
        contains(REASON_MUST_BE_NON_NEGATIVE));
  }

  @Test
  public void shouldRejectIfTotalConsumedQuantityIsIncorrectlyCalculated() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setTotalConsumedQuantity(2);
    requisitionLineItems.add(lineItem);

    Message message1 = new Message(RequisitionValidator.VALUE_MUST_BE_ENTERED_NOTIFICATION,
        RequisitionValidator.REQUISITION_LINE_ITEMS);
    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        RequisitionValidator.REQUISITION_LINE_ITEMS
            + ENTERED_PRIOR_SUBMISSION_NOTIFICATION));

    Message message2 = new Message("requisition.error.validation.incorrect-value",
        RequisitionLineItem.STOCK_ON_HAND,
        RequisitionLineItem.TOTAL_CONSUMED_QUANTITY);
    when(messageService.localize(message2)).thenReturn(message2.new LocalizedMessage(
        "has incorrect value, it does not match the calculated value."));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(RequisitionValidator.VALUE_IS_INCORRECTLY_CALCULATED));
  }

  @Test
  public void shouldRejectIfMaximumStockQuantityIsIncorrectlyCalculated() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setMaximumStockQuantity(10);
    lineItem.setCalculatedOrderQuantity(9);
    requisitionLineItems.add(lineItem);

    Message message = new Message(
        "requisition.error.validation.value-does-not-match-calculated-value",
        RequisitionLineItem.MAXIMUM_STOCK_QUANTITY);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        "has incorrect value, it does not match the calculated value."));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(RequisitionValidator.VALUE_IS_INCORRECTLY_CALCULATED));
  }

  @Test
  public void shouldNotValidateSkippedLineItems() {
    Requisition requisition = getRequisition();

    RequisitionLineItem lineItem = getSkippedAndIncorrectRequisitionLineItem();

    RequisitionLineItem lineItem2 = generateLineItem();

    requisitionLineItems.add(lineItem);
    requisitionLineItems.add(lineItem2);

    requisitionValidator.validate(requisition, errors);

    verify(errors, times(0)).rejectValue(any(), any());
  }

  private RequisitionLineItem generateLineItem() {
    RequisitionLineItem lineItem = new RequisitionLineItem();
    lineItem.setRequestedQuantity(1);
    lineItem.setBeginningBalance(1);
    lineItem.setApprovedQuantity(1);
    lineItem.setTotalReceivedQuantity(1);
    lineItem.setStockOnHand(1);
    lineItem.setTotalConsumedQuantity(1);
    lineItem.setTotalLossesAndAdjustments(0);
    lineItem.setTotalStockoutDays(1);
    lineItem.setTotal(1);
    lineItem.setRequisition(requisition);
    lineItem.setMaxMonthsOfStock(BigDecimal.ONE);
    lineItem.setMaximumStockQuantity(1);
    lineItem.setAverageConsumption(1);
    lineItem.setCalculatedOrderQuantity(0);
    lineItem.setStockAdjustments(new ArrayList<>());
    return lineItem;
  }

  private void mockRepositoriesAndObjects() {
    programId = UUID.randomUUID();

    when(requisition.getProgramId()).thenReturn(programId);
    when(requisition.getNonSkippedRequisitionLineItems()).thenReturn(requisitionLineItems);
    when(requisition.getStatus()).thenReturn(RequisitionStatus.AUTHORIZED);
    when(requisition.getTemplate()).thenReturn(requisitionTemplate);

    when(requisitionRepository.findOne(any())).thenReturn(requisition);
    when(configurationSettingService.getBoolValue("skipAuthorization")).thenReturn(false);
    when(stockAdjustmentReasonReferenceDataService.getStockAdjustmentReasonsByProgram(any()))
        .thenReturn(new ArrayList<>());
  }

  private RequisitionLineItem getSkippedAndIncorrectRequisitionLineItem() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setTotalConsumedQuantity(2);
    lineItem.setSkipped(true);
    return lineItem;
  }

  private Requisition getRequisition() {
    Requisition requisition = new Requisition();
    requisition.setRequisitionLineItems(requisitionLineItems);
    requisition.setProgramId(programId);
    requisition.setStatus(RequisitionStatus.INITIATED);
    requisition.setTemplate(requisitionTemplate);
    return requisition;
  }
}
