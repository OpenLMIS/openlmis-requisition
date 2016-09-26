package org.openlmis.fulfillment.exception;

/**
 * Signals an issue with creating an order CSV file.
 */
public class OrderCsvWriteException extends OrderFileException {

  public OrderCsvWriteException(String message, Throwable cause) {
    super(message, cause);
  }
}
