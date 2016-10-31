package org.openlmis.requisition.validate;

import static org.springframework.util.CollectionUtils.isEmpty;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.StockAdjustment;
import org.openlmis.requisition.dto.StockAdjustmentReasonDto;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.service.referencedata.StockAdjustmentReasonReferenceDataService;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

@SuppressWarnings("PMD.TooManyMethods")
@Component
public class RequisitionValidator extends AbstractRequisitionValidator {

  static final String STOCK_ADJUSTMENT_REASON = "reasonId";
  static final String VALUE_MUST_BE_ENTERED_NOTIFICATION =
      " must be entered prior to submission of a requisition.";
  static final String VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION =
      " must be a non-negative value.";
  static final String TEMPLATE_COLUMN_IS_HIDDEN =
      " is hidden in template and should not contain a value.";
  static final String VALUE_IS_INCORRECTLY_CALCULATED = " has incorrect value, it does not match"
      + " the calculated value.";
  static final String VALUE_NOT_FOUND = " could not be found.";

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  @Autowired
  private StockAdjustmentReasonReferenceDataService stockAdjustmentReasonReferenceDataService;

  @Override
  public void validate(Object target, Errors errors) {
    Requisition requisition = (Requisition) target;

    if (isEmpty(requisition.getRequisitionLineItems())) {
      errors.rejectValue(REQUISITION_LINE_ITEMS,
          "A requisitionLineItems" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    } else {
      RequisitionTemplate template = requisitionTemplateRepository.getTemplateForProgram(
          requisition.getProgramId());

      requisition.getRequisitionLineItems()
          .forEach(lineItem ->
              validateRequisitionLineItem(errors, template, requisition, lineItem));
    }
  }

  private void validateRequisitionLineItem(Errors errors, RequisitionTemplate template,
                                           Requisition requisition, RequisitionLineItem item) {

    Map<String, Integer> fieldsForNotNegativeValidation = addFieldsForNotNegativeValidation(item);

    fieldsForNotNegativeValidation.entrySet().forEach(entry -> rejectIfNullOrNegative(errors,
        template, entry.getValue(), entry.getKey()));

    validateApprovedQuantity(errors, template, requisition, item);

    checkTemplate(errors, template, item.getRequestedQuantityExplanation(),
        RequisitionLineItem.REQUESTED_QUANTITY_EXPLANATION);

    validateCalculations(errors, template, item);

    validateStockAdjustments(errors, requisition, item);
  }

  private Map<String, Integer> addFieldsForNotNegativeValidation(RequisitionLineItem
                                                               requisitionLineItem) {
    Map<String, Integer> fieldsForValidation = new HashMap<>();
    fieldsForValidation.put(RequisitionLineItem.REQUESTED_QUANTITY,
        requisitionLineItem.getRequestedQuantity());

    fieldsForValidation.put(RequisitionLineItem.BEGINNING_BALANCE,
        requisitionLineItem.getBeginningBalance());

    fieldsForValidation.put(RequisitionLineItem.STOCK_ON_HAND,
        requisitionLineItem.getStockOnHand());

    fieldsForValidation.put(RequisitionLineItem.TOTAL_CONSUMED_QUANTITY,
        requisitionLineItem.getTotalConsumedQuantity());

    fieldsForValidation.put(RequisitionLineItem.TOTAL,
        requisitionLineItem.getTotal());

    fieldsForValidation.put(RequisitionLineItem.TOTAL_STOCKOUT_DAYS,
        requisitionLineItem.getTotalStockoutDays());

    fieldsForValidation.put(RequisitionLineItem.TOTAL_RECEIVED_QUANTITY,
        requisitionLineItem.getTotalReceivedQuantity());

    return fieldsForValidation;

  }

  private void rejectIfNullOrNegative(Errors errors, RequisitionTemplate template,
                              Integer value, String field) {
    rejectIfLessThanZero(errors, template, value, field);
    rejectIfNull(errors, template, value, field);
  }

  private void rejectIfLessThanZero(Errors errors, RequisitionTemplate template,
                                    Integer value, String field) {
    boolean templateValid = checkTemplate(errors, template, value, field);

    if (templateValid && value != null && value < 0) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, field + VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION);
    }
  }

  private void rejectIfNull(Errors errors, RequisitionTemplate template,
                            Object value, String field) {
    boolean templateValid = checkTemplate(errors, template, value, field);

    if (templateValid && value == null) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, field + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    }
  }

  private void validateStockAdjustments(
      Errors errors, Requisition requisition, RequisitionLineItem item) {
    List<UUID> reasons = stockAdjustmentReasonReferenceDataService
        .getStockAdjustmentReasonsByProgram(requisition.getProgramId())
        .stream().map(StockAdjustmentReasonDto::getId).collect(Collectors.toList());

    for (StockAdjustment adjustment : item.getStockAdjustments()) {
      if (!reasons.contains(adjustment.getReasonId())) {
        errors.rejectValue(STOCK_ADJUSTMENT_REASON, STOCK_ADJUSTMENT_REASON + " with id "
            + adjustment.getReasonId() + VALUE_NOT_FOUND);
      }

      if (adjustment.getQuantity() == null || adjustment.getQuantity() < 0) {
        errors.rejectValue(STOCK_ADJUSTMENT_REASON, STOCK_ADJUSTMENT_REASON + " with id "
            + adjustment.getReasonId() + VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION);
      }
    }
  }

  private void validateApprovedQuantity(Errors errors, RequisitionTemplate template,
                                        Requisition requisition, RequisitionLineItem item) {
    if (requisition.getStatus() == RequisitionStatus.AUTHORIZED
        || (configurationSettingService.getBoolValue("skipAuthorization")
        && requisition.getStatus() == RequisitionStatus.SUBMITTED)) {

      rejectIfNull(errors, template, item.getApprovedQuantity(),
          RequisitionLineItem.APPROVED_QUANTITY);
      rejectIfLessThanZero(errors, template, item.getApprovedQuantity(),
          RequisitionLineItem.APPROVED_QUANTITY);
    }
  }

  private void validateCalculations(Errors errors, RequisitionTemplate template,
                                   RequisitionLineItem item) {
    boolean templateValid = checkTemplate(errors, template, item.getStockOnHand(),
        RequisitionLineItem.STOCK_ON_HAND) && checkTemplate(errors, template,
        item.getTotalConsumedQuantity(), RequisitionLineItem.TOTAL_CONSUMED_QUANTITY);

    if (templateValid && !item.calculateStockOnHandValue().equals(item.getStockOnHand())) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, RequisitionLineItem.STOCK_ON_HAND + " or "
          + RequisitionLineItem.TOTAL_CONSUMED_QUANTITY  + VALUE_IS_INCORRECTLY_CALCULATED);
    }
  }

  private boolean checkTemplate(Errors errors, RequisitionTemplate template,
                                Object value, String field) {
    try {
      return checkIfDisplayed(errors, template, value, field);
    } catch (RequisitionTemplateColumnException ex) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, ex.getMessage());
    }

    return false;
  }

  private boolean checkIfDisplayed(Errors errors, RequisitionTemplate template, Object value,
                                   String field) throws RequisitionTemplateColumnException {
    if (!template.isColumnDisplayed(field)) {
      if (value != null) {
        errors.rejectValue(REQUISITION_LINE_ITEMS, field + TEMPLATE_COLUMN_IS_HIDDEN);
      }

      return false;
    }

    return true;
  }
}
