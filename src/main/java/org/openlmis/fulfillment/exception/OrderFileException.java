package org.openlmis.fulfillment.exception;

/**
 * Signals an issue with creating the order file.
 */
public abstract class OrderFileException extends Exception {

  public OrderFileException(String message, Throwable cause) {
    super(message, cause);
  }
}
