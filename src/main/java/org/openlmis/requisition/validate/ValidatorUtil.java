package org.openlmis.requisition.validate;

import org.openlmis.utils.Message;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
public class ValidatorUtil {

  public void rejectValue(Errors errors, String field, Message.LocalizedMessage message) {
    errors.rejectValue(field, message.toString());
  }
}
