package org.openlmis.requisition.exception;

/**
 * Signals that an operation was attempted on a requisition with a bad status,
 * i.e. approving of initiated requisition.
 */
public class InvalidRequisitionStatusException extends RequisitionException {

  public InvalidRequisitionStatusException(String message) {
    super(message);
  }
}
