package org.openlmis.requisition.exception;

import org.openlmis.utils.Message;

public class ValidationMessageException extends RuntimeException {

  private final Message message;

  public ValidationMessageException(Message message) {
    super();
    this.message = message;
  }

  public Message asMessage() {
    return message;
  }

  /**
   * Overrides RuntimeException's public String getMessage().
   *
   * @return a localized string description
   */
  @Override
  public String getMessage() {
    return this.message.toString();
  }
}
