package org.openlmis.requisition.validate;

import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateCalculatedOrderQuantity;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateMaximumStockQuantity;
import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateStockOnHand;
import static org.openlmis.requisition.domain.RequisitionLineItem.APPROVED_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.BEGINNING_BALANCE;
import static org.openlmis.requisition.domain.RequisitionLineItem.CALCULATED_ORDER_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.MAXIMUM_STOCK_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.NUMBER_OF_NEW_PATIENTS_ADDED;
import static org.openlmis.requisition.domain.RequisitionLineItem.REQUESTED_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.REQUESTED_QUANTITY_EXPLANATION;
import static org.openlmis.requisition.domain.RequisitionLineItem.STOCK_ON_HAND;
import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL_COLUMN;
import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL_CONSUMED_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL_RECEIVED_QUANTITY;
import static org.openlmis.requisition.domain.RequisitionLineItem.TOTAL_STOCKOUT_DAYS;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_INCORRECT_VALUE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_STOCK_ADJUSTMENT_NON_NEGATIVE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_STOCK_ADJUSTMENT_NOT_FOUND;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALUE_DOES_NOT_MATCH_CALCULATED_VALUE;
import static org.openlmis.requisition.i18n.MessageKeys.ERROR_VALUE_MUST_BE_ENTERED;
import static org.springframework.util.CollectionUtils.isEmpty;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.StockAdjustment;
import org.openlmis.requisition.dto.StockAdjustmentReasonDto;
import org.openlmis.requisition.service.referencedata.StockAdjustmentReasonReferenceDataService;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

@Component
public class RequisitionValidator extends AbstractRequisitionValidator {

  static final String STOCK_ADJUSTMENT_REASON = "reasonId";
  static final String VALUE_IS_INCORRECTLY_CALCULATED = " has incorrect value, it does not match"
      + " the calculated value.";
  static final String VALUE_NOT_FOUND = " could not be found.";

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  @Autowired
  private StockAdjustmentReasonReferenceDataService stockAdjustmentReasonReferenceDataService;

  @Override
  public void validate(Object target, Errors errors) {
    Requisition requisition = (Requisition) target;

    if (isEmpty(requisition.getNonSkippedRequisitionLineItems())) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, messageService.localize(
          new Message(ERROR_VALUE_MUST_BE_ENTERED, REQUISITION_LINE_ITEMS)).toString());
    } else {
      requisition.getNonSkippedRequisitionLineItems()
          .forEach(lineItem ->
              validateRequisitionLineItem(errors, requisition, lineItem));
    }
  }

  private void validateRequisitionLineItem(
      Errors errors, Requisition requisition, RequisitionLineItem item) {
    RequisitionTemplate template = requisition.getTemplate();
    rejectIfNullOrNegative(errors, template, item.getRequestedQuantity(),
        REQUESTED_QUANTITY);

    rejectIfNullOrNegative(errors, template, item.getBeginningBalance(),
        BEGINNING_BALANCE);

    rejectIfNullOrNegative(errors, template, item.getTotalReceivedQuantity(),
        TOTAL_RECEIVED_QUANTITY);

    rejectIfNullOrNegative(errors, template, item.getStockOnHand(),
        STOCK_ON_HAND);

    rejectIfNullOrNegative(errors, template, item.getTotalConsumedQuantity(),
        TOTAL_CONSUMED_QUANTITY);

    rejectIfNullOrNegative(errors, template, item.getTotalStockoutDays(),
        TOTAL_STOCKOUT_DAYS);

    rejectIfNullOrNegative(errors, template, item.getTotal(), TOTAL_COLUMN);

    validateApprovedQuantity(errors, template, requisition, item);

    checkTemplate(errors, template, item.getRequestedQuantityExplanation(),
        REQUESTED_QUANTITY_EXPLANATION);

    rejectIfLessThanZero(errors, template, item.getNumberOfNewPatientsAdded(),
        NUMBER_OF_NEW_PATIENTS_ADDED);

    validateCalculations(errors, template, item);

    validateStockAdjustments(errors, requisition, item);
  }

  private void validateStockAdjustments(
      Errors errors, Requisition requisition, RequisitionLineItem item) {
    List<UUID> reasons = stockAdjustmentReasonReferenceDataService
        .getStockAdjustmentReasonsByProgram(requisition.getProgramId())
        .stream().map(StockAdjustmentReasonDto::getId).collect(Collectors.toList());

    for (StockAdjustment adjustment : item.getStockAdjustments()) {
      if (!reasons.contains(adjustment.getReasonId())) {
        errors.rejectValue(STOCK_ADJUSTMENT_REASON, messageService.localize(
            new Message(ERROR_STOCK_ADJUSTMENT_NOT_FOUND, adjustment.getReasonId())).toString());
      }

      if (adjustment.getQuantity() == null || adjustment.getQuantity() < 0) {
        errors.rejectValue(STOCK_ADJUSTMENT_REASON, messageService.localize(
            new Message(ERROR_STOCK_ADJUSTMENT_NON_NEGATIVE, adjustment.getReasonId())).toString());
      }
    }
  }

  private void validateApprovedQuantity(Errors errors, RequisitionTemplate template,
                                        Requisition requisition, RequisitionLineItem item) {
    if (requisition.getStatus() == RequisitionStatus.AUTHORIZED
        || (configurationSettingService.getBoolValue("skipAuthorization")
        && requisition.getStatus() == RequisitionStatus.SUBMITTED)) {

      rejectIfNull(errors, template, item.getApprovedQuantity(),
          APPROVED_QUANTITY);
      rejectIfLessThanZero(errors, template, item.getApprovedQuantity(),
          APPROVED_QUANTITY);
    }
  }

  private void validateCalculations(Errors errors, RequisitionTemplate template,
                                    RequisitionLineItem item) {
    boolean templateValid = checkTemplate(errors, template, item.getStockOnHand(),
        STOCK_ON_HAND) && checkTemplate(errors, template,
        item.getTotalConsumedQuantity(), TOTAL_CONSUMED_QUANTITY);

    if (templateValid && !Objects.equals(item.getStockOnHand(), calculateStockOnHand(item))) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, messageService.localize(
          new Message(ERROR_INCORRECT_VALUE, RequisitionLineItem.STOCK_ON_HAND,
              RequisitionLineItem.TOTAL_CONSUMED_QUANTITY)).toString());
    }

    if (checkTemplate(errors, template, item.getMaximumStockQuantity(), MAXIMUM_STOCK_QUANTITY)
        && !Objects.equals(item.getMaximumStockQuantity(), calculateMaximumStockQuantity(item,
        template))) {
      errors.rejectValue(
          REQUISITION_LINE_ITEMS, messageService.localize(new Message(
              ERROR_VALUE_DOES_NOT_MATCH_CALCULATED_VALUE,
              MAXIMUM_STOCK_QUANTITY)).toString());
    }

    if (checkTemplate(errors, template, item.getCalculatedOrderQuantity(),
        CALCULATED_ORDER_QUANTITY) && !Objects.equals(item.getCalculatedOrderQuantity(),
        calculateCalculatedOrderQuantity(item, template))) {
      errors.rejectValue(
          REQUISITION_LINE_ITEMS, messageService.localize(new Message(
              ERROR_VALUE_DOES_NOT_MATCH_CALCULATED_VALUE,
              CALCULATED_ORDER_QUANTITY)).toString());
    }
  }

}
