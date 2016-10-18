package org.openlmis.requisition.validate;

import static org.apache.commons.lang.StringUtils.isBlank;
import static org.springframework.util.CollectionUtils.isEmpty;

import org.bouncycastle.ocsp.Req;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;

@Component
public class RequisitionValidator implements Validator {

  static final String VALUE_MUST_BE_ENTERED_NOTIFICATION =
      " must be entered prior to submission of a requisition.";
  static final String VALUE_MUST_BE_NON_NEGATIVE_NOTIFICATION =
      " must be a non-negative value.";
  static final String VALUE_CANNOT_BE_ENTERED_NOTIFICATION =
      " cannot be entered.";
  static final String TEMPLATE_COLUMN_IS_HIDDEN =
      " is hidden in template and should not contain a value.";
  static final String TEMPLATE_COLUMN_IS_CALCULATED =
      " is calculated and should not contain a value";
  static final String EXPLANATION_MUST_BE_ENTERED =
      " must be entered when requested quantity is not empty.";
  static final String IS_ONLY_AVAILABLE_DURING_APPROVAL_STEP =
      " is only available during the approval step of the requisition process.";
  static final String CANNOT_BE_CHANGED = "cannot be changed";

  static final String REQUISITION_LINE_ITEMS = "requisitionLineItems";
  static final String REQUISITION_FIELD = "requisition field";

  static final String REQUESTED_QUANTITY = "requestedQuantity";
  static final String REQUESTED_QUANTITY_EXPLANATION = "requestedQuantityExplanation";
  static final String BEGINNING_BALANCE = "beginningBalance";
  static final String TOTAL_RECEIVED_QUANTITY = "totalReceivedQuantity";
  static final String STOCK_ON_HAND = "stockOnHand";
  static final String TOTAL_CONSUMED_QUANTITY = "totalConsumedQuantity";
  static final String TOTAL_LOSSES_AND_ADJUSTMENTS = "totalLossesAndAdjustments";
  static final String APPROVED_QUANTITY = "approvedQuantity";
  static final String STOCK_IN_HAND = "stockInHand";
  static final String ORDERABLE_PRODUCT_ID = "orderableProductId";

  static final String FACILITY_ID = "facilityId";
  static final String PROCESSING_PERIOD_ID = "processingPeriodId";
  static final String PROGRAM_ID = "programId";
  static final String SUPPLYING_FACILITY_ID = "supplyingProgramId";
  static final String EMERGENCY = "emergancy";
  static final String REMARKS = "Remarks";

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private RequisitionRepository requisitionRepository;


  @Override
  public boolean supports(Class<?> clazz) {
    return Requisition.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    Requisition requisition = (Requisition) target;
    Requisition oldRequisition = requisitionRepository.findOne(requisition.getId());

    if (isEmpty(requisition.getRequisitionLineItems())) {
      errors.rejectValue(REQUISITION_LINE_ITEMS,
          "A requisitionLineItems" + VALUE_MUST_BE_ENTERED_NOTIFICATION);
    } else {
      RequisitionTemplate template = requisitionTemplateRepository.getTemplateForProgram(
          requisition.getProgramId()
      );
      validateImmutableFields(errors, template, requisition, oldRequisition);

      requisition.getRequisitionLineItems()
          .forEach(i -> {
            validateRequisitionLineItem(errors, template, requisition, i);
            validateRequisitionLineItemStateField(errors, template, requisition, i, oldRequisition);
          });
    }
  }

  private void validateImmutableFields(Errors errors, RequisitionTemplate template,
                                       Requisition newRequisition,
                                       Requisition oldRequisition) {
    rejectIfChanged(errors, template, newRequisition.getFacilityId(), oldRequisition
        .getFacilityId(), FACILITY_ID);
    rejectIfChanged(errors, template, newRequisition.getProcessingPeriodId(), oldRequisition
        .getProcessingPeriodId(),
        PROCESSING_PERIOD_ID);
    rejectIfChanged(errors, template, newRequisition.getProgramId(), oldRequisition
        .getProgramId(),
        PROGRAM_ID);
    rejectIfChanged(errors, template, newRequisition.getSupplyingFacilityId(), oldRequisition
        .getSupplyingFacilityId(),
        SUPPLYING_FACILITY_ID);
    rejectIfChanged(errors, template, newRequisition.getEmergency(), oldRequisition
        .getEmergency(),
        EMERGENCY);
  }

  private void rejectIfChanged(Errors errors, RequisitionTemplate template,  Object oldValue,
                               Object newValue,
                               String field) {
    boolean templateValid = checkTemplate(errors, template, newValue, field);
    if (templateValid && !oldValue.equals(newValue)) {
      errors.rejectValue(REQUISITION_FIELD, field + CANNOT_BE_CHANGED);
    }
  }

  private void validateRequisitionLineItemStateField(Errors errors, RequisitionTemplate template,
                                                     Requisition requisition,
                                                     RequisitionLineItem requisitionLineItem,
                                                     Requisition oldRequisition ) {
    List<RequisitionLineItem> oldLineItems = oldRequisition.getRequisitionLineItems();
    RequisitionLineItem oldRequisitionLineItem = null;
    for (RequisitionLineItem oldLineItem : oldLineItems) {
      if (oldLineItem.getId().equals(requisitionLineItem.getId())) {
        oldRequisitionLineItem = oldLineItem;
      }
    }
    if (requisition.getStatus() == RequisitionStatus.SUBMITTED
        || requisition.getStatus() == RequisitionStatus.INITIATED) {
      validateFieldsIfSubmittedOrInitiated(errors, template, requisitionLineItem,
          oldRequisitionLineItem);
    }

    if (requisition.getStatus() == RequisitionStatus.AUTHORIZED) {
      validateFieldsIfAuthorized(errors, template, requisitionLineItem, oldRequisitionLineItem);
    }
  }

  private void validateFieldsIfSubmittedOrInitiated(Errors errors, RequisitionTemplate template,
                                                    RequisitionLineItem requisitionLineItem,
                                                    RequisitionLineItem oldRequisitionLineItem) {
    if (oldRequisitionLineItem == null) {
      rejectIfNotNull(errors, template, requisitionLineItem.getRemarks(), REMARKS);
      rejectIfNotNull(errors, template,
          requisitionLineItem.getApprovedQuantity(), APPROVED_QUANTITY);
      return;
    }
    rejectIfChanged(errors, template, oldRequisitionLineItem.getRemarks(),
        requisitionLineItem.getRemarks(), REMARKS);
    rejectIfChanged(errors, template, oldRequisitionLineItem.getApprovedQuantity(),
        requisitionLineItem.getApprovedQuantity(), APPROVED_QUANTITY);
  }

  private void validateFieldsIfAuthorized(Errors errors, RequisitionTemplate template,
                                          RequisitionLineItem requisitionLineItem,
                                          RequisitionLineItem oldRequisitionLineItem) {
    if (oldRequisitionLineItem == null) {
      rejectIfNotNull(errors, template, requisitionLineItem.getOrderableProductId(),
          ORDERABLE_PRODUCT_ID);
      rejectIfNotNull(errors, template, requisitionLineItem.getStockInHand(), STOCK_IN_HAND);
      rejectIfNotNull(errors, template, requisitionLineItem.getBeginningBalance(),
          BEGINNING_BALANCE);
      rejectIfNotNull(errors, template, requisitionLineItem.getTotalReceivedQuantity(),
          TOTAL_RECEIVED_QUANTITY);
      rejectIfNotNull(errors, template, requisitionLineItem.getTotalLossesAndAdjustments(),
          TOTAL_LOSSES_AND_ADJUSTMENTS);
      rejectIfNotNull(errors, template, requisitionLineItem.getStockOnHand(), STOCK_ON_HAND);
      rejectIfNotNull(errors, template, requisitionLineItem.getRequestedQuantity(),
          REQUESTED_QUANTITY);
      rejectIfNotNull(errors, template, requisitionLineItem.getTotalConsumedQuantity(),
          TOTAL_CONSUMED_QUANTITY);
      rejectIfNotNull(errors, template, requisitionLineItem.getRequestedQuantityExplanation(),
          REQUESTED_QUANTITY_EXPLANATION);
      return;
    }
    rejectIfChanged(errors, template, oldRequisitionLineItem.getOrderableProductId(),
        requisitionLineItem.getOrderableProductId(), ORDERABLE_PRODUCT_ID);
    rejectIfChanged(errors, template, oldRequisitionLineItem.getStockInHand(),
        requisitionLineItem.getStockInHand(), STOCK_IN_HAND);
    rejectIfChanged(errors, template, oldRequisitionLineItem.getBeginningBalance(),
        requisitionLineItem.getBeginningBalance(), BEGINNING_BALANCE);
    rejectIfChanged(errors, template, oldRequisitionLineItem.getTotalReceivedQuantity(),
        requisitionLineItem.getTotalReceivedQuantity(), TOTAL_RECEIVED_QUANTITY);
    rejectIfChanged(errors, template, oldRequisitionLineItem.getTotalLossesAndAdjustments(),
        requisitionLineItem.getTotalLossesAndAdjustments(), TOTAL_LOSSES_AND_ADJUSTMENTS);
    rejectIfChanged(errors, template, oldRequisitionLineItem.getStockOnHand(),
        requisitionLineItem.getStockOnHand(), STOCK_ON_HAND);
    rejectIfChanged(errors, template, oldRequisitionLineItem.getRequestedQuantity(),
        requisitionLineItem.getRequestedQuantity(), REQUESTED_QUANTITY);
    rejectIfChanged(errors, template, oldRequisitionLineItem.getTotalConsumedQuantity(),
        requisitionLineItem.getTotalConsumedQuantity(), TOTAL_CONSUMED_QUANTITY);
    rejectIfChanged(errors, template, oldRequisitionLineItem.getRequestedQuantityExplanation(),
        requisitionLineItem.getRequestedQuantityExplanation(), REQUESTED_QUANTITY_EXPLANATION);
  }

  private void validateRequisitionLineItem(Errors errors, RequisitionTemplate template,
                                           Requisition requisition, RequisitionLineItem item) {
    rejectIfNull(errors, template, item.getRequestedQuantity(), REQUESTED_QUANTITY);
    rejectIfLessThanZero(errors, template, item.getRequestedQuantity(), REQUESTED_QUANTITY);

    rejectIfNull(errors, template, item.getBeginningBalance(), BEGINNING_BALANCE);
    rejectIfLessThanZero(errors, template, item.getBeginningBalance(), BEGINNING_BALANCE);

    rejectIfNull(errors, template, item.getTotalReceivedQuantity(), TOTAL_RECEIVED_QUANTITY);
    rejectIfLessThanZero(
        errors, template, item.getTotalReceivedQuantity(), TOTAL_RECEIVED_QUANTITY
    );

    rejectIfNull(errors, template, item.getStockOnHand(), STOCK_ON_HAND);

    rejectIfNull(errors, template, item.getTotalConsumedQuantity(), TOTAL_CONSUMED_QUANTITY);
    rejectIfLessThanZero(
        errors, template, item.getTotalConsumedQuantity(), TOTAL_CONSUMED_QUANTITY
    );

    rejectIfNull(
        errors, template, item.getTotalLossesAndAdjustments(), TOTAL_LOSSES_AND_ADJUSTMENTS
    );

    validateApprovedQuantity(errors, template, requisition, item);

    validateRequestedQuantityExplanation(errors, template, item);
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

  private void rejectIfNotNull(Errors errors, RequisitionTemplate template,
                               Object value, String field) {
    boolean templateValid = checkTemplate(errors, template, value, field);

    if (templateValid && value != null) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, field + VALUE_CANNOT_BE_ENTERED_NOTIFICATION);
    }
  }

  private void validateApprovedQuantity(Errors errors, RequisitionTemplate template,
                                        Requisition requisition, RequisitionLineItem item) {
    Integer value = item.getApprovedQuantity();

    if (requisition.getStatus() == RequisitionStatus.AUTHORIZED) {
      rejectIfNull(errors, template, item.getTotalConsumedQuantity(), TOTAL_CONSUMED_QUANTITY);
      rejectIfLessThanZero(
          errors, template, item.getTotalConsumedQuantity(), TOTAL_CONSUMED_QUANTITY
      );
    } else if (value != null) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, APPROVED_QUANTITY
          + IS_ONLY_AVAILABLE_DURING_APPROVAL_STEP);
    }
  }

  private void validateRequestedQuantityExplanation(Errors errors, RequisitionTemplate template,
                                                    RequisitionLineItem item) {
    String value = item.getRequestedQuantityExplanation();
    boolean templateValid = checkTemplate(errors, template, value, REQUESTED_QUANTITY_EXPLANATION);

    if (templateValid && item.getRequestedQuantity() != null && isBlank(value)) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, REQUESTED_QUANTITY_EXPLANATION
          + EXPLANATION_MUST_BE_ENTERED);
    }
  }

  private boolean checkTemplate(Errors errors, RequisitionTemplate template,
                                Object value, String field) {
    try {
      boolean displayed = checkIfDisplayed(errors, template, value, field);
      boolean calculated = checkIfCalculated(errors, template, value, field);

      return displayed && !calculated;
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

  private boolean checkIfCalculated(Errors errors, RequisitionTemplate template, Object value,
                                    String field) throws RequisitionTemplateColumnException {
    if (template.isColumnCalculated(field)) {
      if (value != null) {
        errors.rejectValue(REQUISITION_LINE_ITEMS, field + TEMPLATE_COLUMN_IS_CALCULATED);
      }

      return true;
    }

    return false;
  }
}
