package org.openlmis.requisition.validate;

import static org.springframework.util.CollectionUtils.isEmpty;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.exception.RequisitionTemplateColumnException;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
public class DraftRequisitionValidator implements Validator {

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Override
  public boolean supports(Class<?> clazz) {
    return Requisition.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    Requisition requisition = (Requisition) target;

    if (!isEmpty(requisition.getRequisitionLineItems())) {
      RequisitionTemplate template = requisitionTemplateRepository.getTemplateForProgram(
          requisition.getProgramId()
      );
      requisition.getRequisitionLineItems()
          .forEach(i -> validateRequisitionLineItem(errors, template, i));
    }
  }

  private void validateRequisitionLineItem(Errors errors, RequisitionTemplate template,
                                           RequisitionLineItem item) {
    rejectIfCalculatedAndNotNull(errors, template, item.getStockOnHand(),
        RequisitionValidator.STOCK_ON_HAND);
    rejectIfCalculatedAndNotNull(errors, template, item.getTotalConsumedQuantity(),
        RequisitionValidator.TOTAL_CONSUMED_QUANTITY);

  }

  private void rejectIfCalculatedAndNotNull(Errors errors, RequisitionTemplate template,
                                            Object value, String field) {
    try {
      if (template.isColumnCalculated(field) && value != null) {
        errors.rejectValue(RequisitionValidator.REQUISITION_LINE_ITEMS,
            field + RequisitionValidator.TEMPLATE_COLUMN_IS_CALCULATED);
      }
    } catch (RequisitionTemplateColumnException ex) {
      errors.rejectValue(RequisitionValidator.REQUISITION_LINE_ITEMS, ex.getMessage());
    }
  }
}
