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

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Component
public class RequisitionValidator extends AbstractRequisitionValidator {

  static final String STOCK_ADJUSTMENT_REASON = "reasonId";
  static final String VALUE_MUST_BE_ENTERED_NOTIFICATION =
      " must be entered prior to submission of a requisition.";
  static final String VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION =
      " must be a non-negative value.";
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
          requisition.getProgramId()
      );

      requisition.getRequisitionLineItems()
          .forEach(i -> validateRequisitionLineItem(errors, template, requisition, i));
    }
  }

  private void validateRequisitionLineItem(Errors errors, RequisitionTemplate template,
                                           Requisition requisition, RequisitionLineItem item) {
    rejectIfDisplayedAndNull(errors, template, item.getRequestedQuantity(),
        RequisitionLineItem.REQUESTED_QUANTITY);
    rejectIfLessThanZero(errors, template, item.getRequestedQuantity(),
        RequisitionLineItem.REQUESTED_QUANTITY);

    rejectIfDisplayedAndNull(errors, template, item.getBeginningBalance(),
        RequisitionLineItem.BEGINNING_BALANCE);
    rejectIfLessThanZero(errors, template, item.getBeginningBalance(),
        RequisitionLineItem.BEGINNING_BALANCE);

    rejectIfDisplayedAndNull(errors, template, item.getTotalReceivedQuantity(),
        RequisitionLineItem.TOTAL_RECEIVED_QUANTITY);
    rejectIfLessThanZero(errors, template, item.getTotalReceivedQuantity(),
        RequisitionLineItem.TOTAL_RECEIVED_QUANTITY);

    rejectIfDisplayedAndNull(errors, template, item.getStockOnHand(),
        RequisitionLineItem.STOCK_ON_HAND);
    rejectIfLessThanZero(errors, template, item.getStockOnHand(),
        RequisitionLineItem.STOCK_ON_HAND);

    rejectIfDisplayedAndNull(errors, template, item.getTotalConsumedQuantity(),
        RequisitionLineItem.TOTAL_CONSUMED_QUANTITY);
    rejectIfLessThanZero(errors, template, item.getTotalConsumedQuantity(),
        RequisitionLineItem.TOTAL_CONSUMED_QUANTITY);

    validateApprovedQuantity(errors, template, requisition, item);

    validateCalculations(errors, template, item);

    validateStockAdjustments(errors, requisition, item);
  }

  private void rejectIfLessThanZero(Errors errors, RequisitionTemplate template,
                                    Integer value, String field) {
    boolean displayed = checkIfDisplayed(errors, template, field);

    if (displayed && value != null && value < 0) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, field + VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION);
    }
  }

  private void rejectIfDisplayedAndNull(Errors errors, RequisitionTemplate template,
                                        Object value, String field) {
    boolean displayed = checkIfDisplayed(errors, template, field);

    if (displayed && value == null) {
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

      rejectIfDisplayedAndNull(errors, template, item.getApprovedQuantity(),
          RequisitionLineItem.APPROVED_QUANTITY);
      rejectIfLessThanZero(errors, template, item.getApprovedQuantity(),
          RequisitionLineItem.APPROVED_QUANTITY);
    }
  }

  private void validateCalculations(Errors errors, RequisitionTemplate template,
                                   RequisitionLineItem item) {
    boolean displayed = checkIfDisplayed(errors, template, RequisitionLineItem.STOCK_ON_HAND)
        && checkIfDisplayed(errors, template, RequisitionLineItem.TOTAL_CONSUMED_QUANTITY);

    if (displayed && !item.calculateStockOnHandValue().equals(item.getStockOnHand())) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, RequisitionLineItem.STOCK_ON_HAND + " or "
          + RequisitionLineItem.TOTAL_CONSUMED_QUANTITY  + VALUE_IS_INCORRECTLY_CALCULATED);
    }
  }

  private boolean checkIfDisplayed(Errors errors, RequisitionTemplate template, String field) {
    try {
      return template.isColumnDisplayed(field);
    } catch (RequisitionTemplateColumnException ex) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, ex.getMessage());
    }

    return false;
  }
}
