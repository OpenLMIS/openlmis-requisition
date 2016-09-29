package org.openlmis.fulfillment.exception;

/**
 * Signals an issues with the creation of an order PDF file.
 */
public class OrderPdfWriteException extends OrderFileException {

  public OrderPdfWriteException(String message, Throwable cause) {
    super(message, cause);
  }
}
