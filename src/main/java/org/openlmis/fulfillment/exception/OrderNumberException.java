package org.openlmis.fulfillment.exception;


/**
 * Signals an issue with generating order number.
 */
public class OrderNumberException extends RuntimeException {

  public OrderNumberException(String message) {
    super(message);
  }
}
