package org.openlmis.requisition.validate;

import org.openlmis.requisition.domain.Requisition;
import org.springframework.validation.Validator;

abstract class AbstractRequisitionValidator implements Validator {

  static final String TEMPLATE_COLUMN_IS_CALCULATED =
      " is calculated and should not contain a value";
  static final String IS_ONLY_AVAILABLE_DURING_APPROVAL_STEP =
      " is only available during the approval step of the requisition process.";
  static final String REQUISITION_LINE_ITEMS = "requisitionLineItems";

  public boolean supports(Class<?> clazz) {
    return Requisition.class.equals(clazz);
  }
}
