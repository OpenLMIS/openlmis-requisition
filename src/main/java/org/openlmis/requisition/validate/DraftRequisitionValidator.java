package org.openlmis.requisition.validate;

import static org.springframework.util.CollectionUtils.isEmpty;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.i18n.MessageService;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
public class DraftRequisitionValidator extends AbstractRequisitionValidator {
  @Autowired
  private MessageService messageService;

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

    validateInvariantsDidntChange(errors, requisition, savedRequisition);

    if (!isEmpty(requisition.getNonSkippedRequisitionLineItems())) {
      requisition.getNonSkippedRequisitionLineItems()
          .forEach(i -> validateRequisitionLineItem(errors, savedRequisition, i));
    }
  }

  private void validateInvariantsDidntChange(Errors errors, Requisition requisition,
                                             Requisition savedRequisition) {
    rejectIfValueChanged(errors, requisition.getFacilityId(),
        savedRequisition.getFacilityId(), Requisition.FACILITY_ID);
    rejectIfValueChanged(errors, requisition.getProgramId(),
        savedRequisition.getProgramId(), Requisition.PROGRAM_ID);
    rejectIfValueChanged(errors, requisition.getProcessingPeriodId(),
        savedRequisition.getProcessingPeriodId(), Requisition.PROCESSING_PERIOD_ID);
    rejectIfValueChanged(errors, requisition.getEmergency(),
        savedRequisition.getEmergency(), Requisition.EMERGENCY);
    rejectIfValueChanged(errors, requisition.getCreatorId(),
        savedRequisition.getCreatorId(), Requisition.CREATOR_ID);
  }

  private void validateRequisitionLineItem(
      Errors errors, Requisition requisition, RequisitionLineItem item) {
    RequisitionTemplate template = requisition.getTemplate();
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
        expectedStatus, messageService.localize(new Message(
            IS_ONLY_AVAILABLE_DURING_APPROVAL_STEP,
            RequisitionLineItem.APPROVED_QUANTITY)).toString());

    rejectIfInvalidStatusAndNotNull(errors, requisition, item.getRemarks(),
        expectedStatus, messageService.localize(new Message(
            IS_ONLY_AVAILABLE_DURING_APPROVAL_STEP,
            RequisitionLineItem.REMARKS_COLUMN)).toString());

  }

  private void rejectIfCalculatedAndNotNull(Errors errors, RequisitionTemplate template,
                                            Object value, String field) {
    if (template.isColumnCalculated(field) && value != null) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, messageService.localize(
          new Message(TEMPLATE_COLUMN_IS_CALCULATED, field)).toString());
    }
  }

  private void rejectIfInvalidStatusAndNotNull(Errors errors, Requisition requisition, Object value,
                                               RequisitionStatus expectedStatus, String errorCode) {
    if (requisition.getStatus() != expectedStatus && value != null) {
      errors.rejectValue(REQUISITION_LINE_ITEMS, errorCode);
    }
  }

  private void rejectIfValueChanged(Errors errors, Object value,
                                    Object savedValue, String field) {
    if (savedValue != null && !savedValue.equals(value)) {
      errors.rejectValue(field,
          messageService.localize(new Message(IS_INVARIANT, field)).toString());
    }
  }
}
