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
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.Requisition.DATE_PHYSICAL_STOCK_COUNT_COMPLETED;
import static org.openlmis.requisition.domain.RequisitionLineItem.CALCULATED_ORDER_QUANTITY;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_INCORRECT_VALUE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_IS_HIDDEN;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_MUST_BE_NON_NEGATIVE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_STOCK_ADJUSTMENT_NON_NEGATIVE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_STOCK_ADJUSTMENT_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALUE_DOES_NOT_MATCH_CALCULATED_VALUE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALUE_MUST_BE_ENTERED;

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
import org.openlmis.requisition.utils.DatePhysicalStockCountCompletedEnabledPredicate;
import org.openlmis.requisition.utils.Message;
import org.springframework.validation.Errors;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
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
  private DatePhysicalStockCountCompletedEnabledPredicate predicate;

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

    Message message1 = new Message(ERROR_VALUE_MUST_BE_ENTERED,
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
    lineItem.setTotalStockoutDays(-1);
    requisitionLineItems.add(lineItem);

    Message message1 = new Message(ERROR_VALUE_MUST_BE_ENTERED,
        RequisitionValidator.REQUISITION_LINE_ITEMS);
    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        RequisitionValidator.REQUISITION_LINE_ITEMS
            + ENTERED_PRIOR_SUBMISSION_NOTIFICATION));

    Message message = new Message(ERROR_MUST_BE_NON_NEGATIVE,
        RequisitionLineItem.TOTAL_STOCKOUT_DAYS);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        RequisitionLineItem.TOTAL_STOCKOUT_DAYS
            + " must be a non-negative value."));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(RequisitionLineItem.TOTAL_STOCKOUT_DAYS
            + " must be a non-negative value."));
  }

  @Test
  public void shouldRejectIfApprovedQuantityIsNullForApprovableState() {
    when(requisition.isApprovable()).thenReturn(true);

    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setApprovedQuantity(null);
    requisitionLineItems.add(lineItem);

    Message message = new Message(ERROR_VALUE_MUST_BE_ENTERED,
        RequisitionLineItem.APPROVED_QUANTITY);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        RequisitionLineItem.APPROVED_QUANTITY
            + ERROR_VALUE_MUST_BE_ENTERED));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(RequisitionLineItem.APPROVED_QUANTITY
            + ERROR_VALUE_MUST_BE_ENTERED));
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

    Message message = new Message(ERROR_IS_HIDDEN, RequisitionLineItem.STOCK_ON_HAND);
    when(messageService.localize(message)).thenReturn(
        message.new LocalizedMessage("is hidden in template and should not contain a value."));

    Message message1 = new Message(ERROR_MUST_BE_NON_NEGATIVE,
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

    Message message1 = new Message(ERROR_MUST_BE_NON_NEGATIVE,
        RequisitionValidator.REQUISITION_LINE_ITEMS);
    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        RequisitionValidator.REQUISITION_LINE_ITEMS
            + ENTERED_PRIOR_SUBMISSION_NOTIFICATION));

    Message message2 = new Message(ERROR_INCORRECT_VALUE, RequisitionLineItem.STOCK_ON_HAND,
        RequisitionLineItem.TOTAL_CONSUMED_QUANTITY);
    when(messageService.localize(message2)).thenReturn(message2.new LocalizedMessage(
        RequisitionValidator.VALUE_IS_INCORRECTLY_CALCULATED));

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

    Message message = new Message(ERROR_STOCK_ADJUSTMENT_NOT_FOUND, adjustment.getReasonId());
    Message message1 = new Message(ERROR_MUST_BE_NON_NEGATIVE,
        RequisitionValidator.REQUISITION_LINE_ITEMS);
    Message message2 = new Message(ERROR_STOCK_ADJUSTMENT_NON_NEGATIVE, adjustment.getReasonId());

    when(adjustment.getId()).thenReturn(UUID.randomUUID());
    when(messageService.localize(message)).thenReturn(
        message.new LocalizedMessage("reasonId with id  could not be found."));
    when(messageService.localize(message2)).thenReturn(
        message2.new LocalizedMessage(REASON_MUST_BE_NON_NEGATIVE));
    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        RequisitionValidator.REQUISITION_LINE_ITEMS
            + ENTERED_PRIOR_SUBMISSION_NOTIFICATION));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(RequisitionValidator.VALUE_NOT_FOUND));
  }

  @Test
  public void shouldRejectIfStockAdjustmentsHaveNullQuantity() {
    StockAdjustment adjustment = mock(StockAdjustment.class);
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.getStockAdjustments().add(adjustment);
    requisitionLineItems.add(lineItem);

    Message message1 = new Message(ERROR_MUST_BE_NON_NEGATIVE,
        RequisitionValidator.REQUISITION_LINE_ITEMS);
    Message message = new Message(ERROR_STOCK_ADJUSTMENT_NOT_FOUND, adjustment.getReasonId());
    Message message2 = new Message(ERROR_STOCK_ADJUSTMENT_NON_NEGATIVE, adjustment.getReasonId());

    when(adjustment.getQuantity()).thenReturn(null);
    when(messageService.localize(message)).thenReturn(
        message.new LocalizedMessage("must be a non-negative value."));
    when(messageService.localize(message2)).thenReturn(
        message2.new LocalizedMessage(REASON_MUST_BE_NON_NEGATIVE));
    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        RequisitionValidator.REQUISITION_LINE_ITEMS
            + ENTERED_PRIOR_SUBMISSION_NOTIFICATION));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(REASON_MUST_BE_NON_NEGATIVE));
  }

  @Test
  public void shouldRejectIfStockAdjustmentsHaveNegativeQuantity() {
    StockAdjustment adjustment = mock(StockAdjustment.class);
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.getStockAdjustments().add(adjustment);
    requisitionLineItems.add(lineItem);

    Message message = new Message(ERROR_STOCK_ADJUSTMENT_NOT_FOUND,  adjustment.getReasonId());
    Message message1 = new Message(ERROR_MUST_BE_NON_NEGATIVE,
        RequisitionValidator.REQUISITION_LINE_ITEMS);
    Message message2 = new Message(ERROR_STOCK_ADJUSTMENT_NON_NEGATIVE, adjustment.getReasonId());

    when(adjustment.getQuantity()).thenReturn(-6);
    when(messageService.localize(message)).thenReturn(
        message.new LocalizedMessage("must be a non-negative value."));
    when(messageService.localize(message2)).thenReturn(
        message2.new LocalizedMessage(REASON_MUST_BE_NON_NEGATIVE));
    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        RequisitionValidator.REQUISITION_LINE_ITEMS
            + ENTERED_PRIOR_SUBMISSION_NOTIFICATION));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(REASON_MUST_BE_NON_NEGATIVE));
  }

  @Test
  public void shouldRejectIfTotalConsumedQuantityIsIncorrectlyCalculated() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setTotalConsumedQuantity(2);
    requisitionLineItems.add(lineItem);

    Message message1 = new Message(ERROR_MUST_BE_NON_NEGATIVE,
        RequisitionValidator.REQUISITION_LINE_ITEMS);
    when(messageService.localize(message1)).thenReturn(message1.new LocalizedMessage(
        RequisitionValidator.REQUISITION_LINE_ITEMS
            + ENTERED_PRIOR_SUBMISSION_NOTIFICATION));

    Message message2 = new Message(ERROR_INCORRECT_VALUE, RequisitionLineItem.STOCK_ON_HAND,
        RequisitionLineItem.TOTAL_CONSUMED_QUANTITY);
    when(messageService.localize(message2)).thenReturn(message2.new LocalizedMessage(
        RequisitionValidator.VALUE_IS_INCORRECTLY_CALCULATED));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(RequisitionValidator.VALUE_IS_INCORRECTLY_CALCULATED));
  }

  @Test
  public void shouldRejectIfMaximumStockQuantityIsIncorrectlyCalculated() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setMaximumStockQuantity(10);
    lineItem.setCalculatedOrderQuantity(9);
    lineItem.setRequestedQuantity(9);
    requisitionLineItems.add(lineItem);

    Message message = new Message(ERROR_VALUE_DOES_NOT_MATCH_CALCULATED_VALUE,
        RequisitionLineItem.MAXIMUM_STOCK_QUANTITY);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        RequisitionValidator.VALUE_IS_INCORRECTLY_CALCULATED));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS),
        contains(RequisitionValidator.VALUE_IS_INCORRECTLY_CALCULATED));
  }

  @Test
  public void shouldRejectIfCalculatedOrderQuantityIsIncorrectlyCalculated() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setCalculatedOrderQuantity(9);
    lineItem.setRequestedQuantity(9);
    requisitionLineItems.add(lineItem);

    Message message = new Message(ERROR_VALUE_DOES_NOT_MATCH_CALCULATED_VALUE,
        CALCULATED_ORDER_QUANTITY);
    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(
        RequisitionValidator.VALUE_IS_INCORRECTLY_CALCULATED));

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

  @Test
  public void shouldRejectIfRequestedQuantityNotEqualsCalculatedAndExplanationIsNotGiven() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(lineItem.getCalculatedOrderQuantity() + 5);
    lineItem.setRequestedQuantityExplanation(null);
    requisitionLineItems.add(lineItem);

    Message message = new Message(ERROR_VALUE_MUST_BE_ENTERED,
        RequisitionLineItem.REQUESTED_QUANTITY_EXPLANATION);
    String msg = "requestedQuantityExplanation required";

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(msg));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS), contains(msg));
  }

  @Test
  public void shouldNotRejectIfRequestedQuantityNotEqualsCalculatedAndExplanationIsGiven() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(lineItem.getCalculatedOrderQuantity() + 5);
    lineItem.setRequestedQuantityExplanation("I need more");
    requisitionLineItems.add(lineItem);

    requisitionValidator.validate(requisition, errors);

    verifyZeroInteractions(errors);
  }

  @Test
  public void shouldNotRejectWhenRequestedQuantityEqualsCalculatedAndExplanationIsNotGiven() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(lineItem.getCalculatedOrderQuantity());
    lineItem.setRequestedQuantityExplanation(null);
    requisitionLineItems.add(lineItem);

    requisitionValidator.validate(requisition, errors);

    verifyZeroInteractions(errors);
  }

  @Test
  public void shouldNotRejectWhenRequestedQuantityIsNullAndExplanationIsNotGiven() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(null);
    lineItem.setRequestedQuantityExplanation(null);
    requisitionLineItems.add(lineItem);

    requisitionValidator.validate(requisition, errors);

    verifyZeroInteractions(errors);
  }

  @Test
  public void shouldNotRejectWhenCalcOrderQuantityNotOnTemplateAndExplanationIsNotGiven() {
    columnsMap.get(RequisitionLineItem.CALCULATED_ORDER_QUANTITY).setIsDisplayed(false);
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(lineItem.getCalculatedOrderQuantity() + 5);
    lineItem.setCalculatedOrderQuantity(null);
    lineItem.setRequestedQuantityExplanation(null);
    requisitionLineItems.add(lineItem);

    requisitionValidator.validate(requisition, errors);

    verifyZeroInteractions(errors);
  }

  @Test
  public void shouldRejectWhenRequestedQuantityIsNullAndCalcOrderQuantityNotOnTemplate() {
    columnsMap.get(RequisitionLineItem.CALCULATED_ORDER_QUANTITY).setIsDisplayed(false);

    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(null);
    lineItem.setCalculatedOrderQuantity(null);
    requisitionLineItems.add(lineItem);

    Message message = new Message(ERROR_VALUE_MUST_BE_ENTERED,
        RequisitionLineItem.REQUESTED_QUANTITY);
    String msg = "requestedQuantity required";

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(msg));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS), contains(msg));
  }

  @Test
  public void shouldNotRejectWhenRequestedQuantityIsNullAndCalcOrderQuantityOnTemplate() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(null);
    requisitionLineItems.add(lineItem);

    requisitionValidator.validate(requisition, errors);

    verifyZeroInteractions(errors);
  }
  
  @Test
  public void shouldNotRejectWhenRequestedQuantityIsNotNullAndCalcOrderQuantityNotOnTemplate() {
    columnsMap.get(RequisitionLineItem.CALCULATED_ORDER_QUANTITY).setIsDisplayed(false);

    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setCalculatedOrderQuantity(null);
    requisitionLineItems.add(lineItem);

    requisitionValidator.validate(requisition, errors);

    verifyZeroInteractions(errors);
  }

  @Test
  public void shouldRejectWhenRequestedQuantityIsLessThanZero() {
    RequisitionLineItem lineItem = generateLineItem();
    lineItem.setRequestedQuantity(-1);
    requisitionLineItems.add(lineItem);

    Message message = new Message(ERROR_MUST_BE_NON_NEGATIVE,
        RequisitionLineItem.REQUESTED_QUANTITY);
    String msg = "requestedQuantity must be non negative";

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(msg));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(RequisitionValidator.REQUISITION_LINE_ITEMS), contains(msg));
  }

  @Test
  public void shouldRejectIfDatePhysicalStockCountCompletedIsNullDuringSubmit() {
    shouldRejectIfDatePhysicalStockCountCompletedIsNullDuring(RequisitionStatus.INITIATED);
  }

  @Test
  public void shouldRejectIfDatePhysicalStockCountCompletedIsNullDuringSubmitAfterReject() {
    shouldRejectIfDatePhysicalStockCountCompletedIsNullDuring(RequisitionStatus.REJECTED);
  }

  @Test
  public void shouldRejectIfDatePhysicalStockCountCompletedIsNullDuringAuthorize() {
    shouldRejectIfDatePhysicalStockCountCompletedIsNullDuring(RequisitionStatus.SUBMITTED);
  }

  @Test
  public void shouldNotRejectIfDatePhysicalStockCountCompletedIsDisabled() {
    when(predicate.exec(any(UUID.class))).thenReturn(false);
    shouldNotRejectIfDatePhysicalStockCountCompletedIsNullDuring(RequisitionStatus.SUBMITTED);
  }

  @Test
  public void shouldNotRejectIfDatePhysicalStockCountCompletedIsNotNullDuringSubmit() {
    shouldNotRejectIfDatePhysicalStockCountCompletedIsNotNullDuring(RequisitionStatus.INITIATED);
    shouldNotRejectIfDatePhysicalStockCountCompletedIsNotNullDuring(RequisitionStatus.REJECTED);
  }

  @Test
  public void shouldNotRejectIfDatePhysicalStockCountCompletedIsNotNullDuringAuthorize() {
    shouldNotRejectIfDatePhysicalStockCountCompletedIsNotNullDuring(RequisitionStatus.SUBMITTED);
  }

  @Test
  public void shouldNotRejectIfDatePhysicalStockCountCompletedIsNullDuringApprove() {
    shouldNotRejectIfDatePhysicalStockCountCompletedIsNullDuring(RequisitionStatus.AUTHORIZED);
  }

  @Test
  public void shouldNotRejectIfDatePhysicalStockCountCompletedIsNullDuringApprovalHierarchy() {
    shouldNotRejectIfDatePhysicalStockCountCompletedIsNullDuring(RequisitionStatus.IN_APPROVAL);
  }

  @Test
  public void shouldNotRejectIfDatePhysicalStockCountCompletedIsNullDuringRelease() {
    shouldNotRejectIfDatePhysicalStockCountCompletedIsNullDuring(RequisitionStatus.APPROVED);
  }

  private void shouldRejectIfDatePhysicalStockCountCompletedIsNullDuring(RequisitionStatus status) {
    RequisitionLineItem lineItem = generateLineItem();
    requisitionLineItems.add(lineItem);

    Requisition requisition = getRequisition();
    requisition.setStatus(status);
    requisition.setDatePhysicalStockCountCompleted(null);
    Message message = new Message(ERROR_VALUE_MUST_BE_ENTERED, DATE_PHYSICAL_STOCK_COUNT_COMPLETED);
    String msg =
        "datePhysicalStockCountCompleted must be entered prior to submission of a requisition";

    when(messageService.localize(message)).thenReturn(message.new LocalizedMessage(msg));

    requisitionValidator.validate(requisition, errors);

    verify(errors).rejectValue(eq(DATE_PHYSICAL_STOCK_COUNT_COMPLETED), contains(msg));
  }

  private void shouldNotRejectIfDatePhysicalStockCountCompletedIsNotNullDuring(
      RequisitionStatus status) {
    RequisitionLineItem lineItem = generateLineItem();
    requisitionLineItems.add(lineItem);

    Requisition requisition = getRequisition();
    requisition.setStatus(status);

    requisitionValidator.validate(requisition, errors);

    verify(errors, times(0)).rejectValue(anyString(), anyString());
  }

  private void shouldNotRejectIfDatePhysicalStockCountCompletedIsNullDuring(
      RequisitionStatus status) {
    RequisitionLineItem lineItem = generateLineItem();
    requisitionLineItems.add(lineItem);

    when(requisition.getStatus()).thenReturn(status);
    when(requisition.getDatePhysicalStockCountCompleted()).thenReturn(null);

    requisitionValidator.validate(requisition, errors);

    verify(errors, times(0)).rejectValue(anyString(), anyString());
  }

  private RequisitionLineItem generateLineItem() {
    RequisitionLineItem lineItem = new RequisitionLineItem();
    lineItem.setRequestedQuantity(1);
    lineItem.setRequestedQuantityExplanation("explanation");
    lineItem.setBeginningBalance(1);
    lineItem.setApprovedQuantity(1);
    lineItem.setTotalReceivedQuantity(1);
    lineItem.setStockOnHand(1);
    lineItem.setTotalConsumedQuantity(1);
    lineItem.setTotalLossesAndAdjustments(0);
    lineItem.setTotalStockoutDays(1);
    lineItem.setTotal(1);
    lineItem.setRequisition(requisition);
    lineItem.setMaxPeriodsOfStock(BigDecimal.ONE);
    lineItem.setMaximumStockQuantity(1);
    lineItem.setAverageConsumption(1);
    lineItem.setCalculatedOrderQuantity(0);
    lineItem.setRequestedQuantity(0);
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
    when(predicate.exec(any(UUID.class))).thenReturn(true);
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
    requisition.setDatePhysicalStockCountCompleted(LocalDate.now());
    requisition.setStockAdjustmentReasons(Collections.emptyList());
    return requisition;
  }
}
