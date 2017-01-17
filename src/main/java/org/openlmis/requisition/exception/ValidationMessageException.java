package org.openlmis.requisition.exception;

import org.openlmis.utils.Message;

public class ValidationMessageException extends BaseMessageException {

  public ValidationMessageException(Message message) {
    super(message);
  }

  public ValidationMessageException(String messageKey) {
    super(messageKey);
  }
}
