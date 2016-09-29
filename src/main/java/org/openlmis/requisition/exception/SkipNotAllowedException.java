package org.openlmis.requisition.exception;

/**
 * Signals that skipping a requisition is not allowed.
 */
public class SkipNotAllowedException extends RequisitionException {

  public SkipNotAllowedException(String message) {
    super(message);
  }
}
