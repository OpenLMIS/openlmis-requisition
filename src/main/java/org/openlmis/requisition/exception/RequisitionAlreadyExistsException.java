package org.openlmis.requisition.exception;

/**
 * Signals that a given requisition already exists.
 */
public class RequisitionAlreadyExistsException extends RequisitionException {

  public RequisitionAlreadyExistsException(String message) {
    super(message);
  }
}
