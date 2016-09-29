package org.openlmis.requisition.exception;

/**
 * Signals that period for the requisition does not belong
 * to a schedule that belongs to the program selected for that requisition.
 */
public class InvalidPeriodException extends RequisitionException {

  public InvalidPeriodException(String message) {
    super(message);
  }
}
