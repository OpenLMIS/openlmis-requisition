package org.openlmis.requisition.exception;

import org.openlmis.utils.Message;

import lombok.Getter;

@Getter
public class BaseLocalizedException extends Exception {
  private final String messageKey;
  private final String[] params;

  /**
   * Creates new Fulfillment exception with message key and params.
   *
   * @param messageKey key that is related with exception message.
   * @param params     params that will be used in the exception message.
   */
  public BaseLocalizedException(String messageKey, String... params) {
    super(messageKey);
    this.messageKey = messageKey;
    this.params = params;
  }

  /**
   * Creates new Fulfillment exception with message key and params.
   *
   * @param cause      the cause.
   * @param messageKey key that is related with exception message.
   * @param params     params that will be used in the exception message.
   */
  public BaseLocalizedException(Throwable cause, String messageKey, String... params) {
    super(messageKey, cause);
    this.messageKey = messageKey;
    this.params = params;
  }

  public Message asMessage() {
    return new Message(messageKey, (Object[])params);
  }

  /**
   * Overrides Exception's public String getMessage().
   *
   * @return a localized string description
   */
  @Override
  public String getMessage() {
    return asMessage().toString();
  }
}
