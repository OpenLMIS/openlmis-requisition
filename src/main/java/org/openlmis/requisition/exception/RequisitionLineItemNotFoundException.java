package org.openlmis.requisition.exception;

/**
 * Signals that a requisition line item was not found.
 */
public class RequisitionLineItemNotFoundException extends RequisitionException {

  public RequisitionLineItemNotFoundException(String message) {
    super(message);
  }
}
