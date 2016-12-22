package org.openlmis.requisition.validate;

import static org.openlmis.requisition.domain.LineItemFieldsCalculator.calculateStockOnHand;
import static org.springframework.util.CollectionUtils.isEmpty;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.StockAdjustment;
import org.openlmis.requisition.dto.StockAdjustmentReasonDto;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.requisition.service.referencedata.StockAdjustmentReasonReferenceDataService;
import org.openlmis.settings.service.ConfigurationSettingService;
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
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  @Autowired
  private StockAdjustmentReasonReferenceDataService stockAdjustmentReasonReferenceDataService;

  @Override
  public void validate(Object target, Errors errors) {
    Requisition requisition = (Requisition) target;

    if (isEmpty(requisition.getNonSkippedRequisitionLineItems())) {
      errors.rejectValue(REQUISITION_LINE_ITEMS,
          "A requisitionLineItems" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    } else {
      RequisitionTemplate template = requisitionTemplateRepository
          .findOne(requisition.getTemplateId());

      requisition.getNonSkippedRequisitionLineItems()
          .forEach(lineItem ->
              validateRequisitionLineItem(errors, template, requisition, lineItem));
    }
  }

  private void validateRequisitionLineItem(Errors errors, RequisitionTemplate template,
                                           Requisition requisition, RequisitionLineItem item) {

    rejectIfNullOrNegative(errors, template, item.getRequestedQuantity(),
        RequisitionLineItem.REQUESTED_QUANTITY);

    rejectIfNullOrNegative(errors, template, item.getBeginningBalance(),
        RequisitionLineItem.BEGINNING_BALANCE);

    rejectIfNullOrNegative(errors, template, item.getTotalReceivedQuantity(),
        RequisitionLineItem.TOTAL_RECEIVED_QUANTITY);

    rejectIfNullOrNegative(errors, template, item.getStockOnHand(),
        RequisitionLineItem.STOCK_ON_HAND);

    rejectIfNullOrNegative(errors, template, item.getTotalConsumedQuantity(),
        RequisitionLineItem.TOTAL_CONSUMED_QUANTITY);

    rejectIfNullOrNegative(errors, template, item.getTotalStockoutDays(),
        RequisitionLineItem.TOTAL_STOCKOUT_DAYS);

    rejectIfNullOrNegative(errors, template, item.getTotal(), RequisitionLineItem.TOTAL_COLUMN);

    validateApprovedQuantity(errors, template, requisition, item);

    checkTemplate(errors, template, item.getRequestedQuantityExplanation(),
        RequisitionLineItem.REQUESTED_QUANTITY_EXPLANATION);

    rejectIfNullOrNegative(errors, template, item.getNumberOfNewPatientsAdded(),
        RequisitionLineItem.NUMBER_OF_NEW_PATIENTS_ADDED);

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

    if (templateValid && !Objects.equals(item.getStockOnHand(), calculateStockOnHand(item))) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, RequisitionLineItem.STOCK_ON_HAND + " or "
          + RequisitionLineItem.TOTAL_CONSUMED_QUANTITY + VALUE_IS_INCORRECTLY_CALCULATED);
    }
  }

}
