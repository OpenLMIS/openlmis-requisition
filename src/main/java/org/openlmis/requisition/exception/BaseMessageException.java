package org.openlmis.requisition.exception;

import org.openlmis.utils.Message;

/**
 * Base class for exceptions using Message.
 */
public class BaseMessageException extends RuntimeException {
  private final Message message;

  public BaseMessageException(Message message) {
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
