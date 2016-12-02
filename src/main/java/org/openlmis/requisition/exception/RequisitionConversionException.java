package org.openlmis.requisition.exception;

/**
 * Exception thrown when occurred error while converting requisition to order.
 */
public class RequisitionConversionException extends RequisitionException {

  public RequisitionConversionException(String message) {
    super(message);
  }
}
