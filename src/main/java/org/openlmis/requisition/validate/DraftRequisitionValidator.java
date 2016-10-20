package org.openlmis.requisition.validate;

import static org.springframework.util.CollectionUtils.isEmpty;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
public class DraftRequisitionValidator extends AbstractRequisitionValidator {

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Override
  public boolean supports(Class<?> clazz) {
    return Requisition.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    Requisition requisition = (Requisition) target;
    Requisition savedRequisition = requisitionRepository.findOne(requisition.getId());

    if (!isEmpty(requisition.getRequisitionLineItems())) {
      RequisitionTemplate template = requisitionTemplateRepository.getTemplateForProgram(
          savedRequisition.getProgramId()
      );
      requisition.getRequisitionLineItems()
          .forEach(i -> validateRequisitionLineItem(errors, template, savedRequisition, i));
    }
  }

  private void validateRequisitionLineItem(Errors errors, RequisitionTemplate template,
                                           Requisition requisition, RequisitionLineItem item) {
    rejectIfCalculatedAndNotNull(errors, template, item.getStockOnHand(),
        RequisitionLineItem.STOCK_ON_HAND);
    rejectIfCalculatedAndNotNull(errors, template, item.getTotalConsumedQuantity(),
        RequisitionLineItem.TOTAL_CONSUMED_QUANTITY);

    validateApprovalFields(errors, requisition, item);
  }

  private void validateApprovalFields(Errors errors, Requisition requisition,
                                      RequisitionLineItem item) {
    RequisitionStatus expectedStatus;
    if (configurationSettingService.getBoolValue("skipAuthorization")) {
      expectedStatus = RequisitionStatus.SUBMITTED;
    } else {
      expectedStatus = RequisitionStatus.AUTHORIZED;
    }
    rejectIfInvalidStatusAndNotNull(errors, requisition, item.getApprovedQuantity(),
        expectedStatus, RequisitionLineItem.APPROVED_QUANTITY
            + IS_ONLY_AVAILABLE_DURING_APPROVAL_STEP);

    rejectIfInvalidStatusAndNotNull(errors, requisition, item.getRemarks(),
        expectedStatus, RequisitionLineItem.REMARKS
            + IS_ONLY_AVAILABLE_DURING_APPROVAL_STEP);

  }

  private void rejectIfCalculatedAndNotNull(Errors errors, RequisitionTemplate template,
                                            Object value, String field) {
    try {
      if (template.isColumnCalculated(field) && value != null) {
        errors.rejectValue(REQUISITION_LINE_ITEMS,
            field + TEMPLATE_COLUMN_IS_CALCULATED);
      }
    } catch (RequisitionTemplateColumnException ex) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, ex.getMessage());
    }
  }

  private void rejectIfInvalidStatusAndNotNull(Errors errors, Requisition requisition, Object value,
                                               RequisitionStatus expectedStatus, String errorCode) {
    if (requisition.getStatus() != expectedStatus && value != null) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, errorCode);
    }
  }
}
