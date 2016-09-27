package org.openlmis.requisition.exception;

public abstract class RequisitionException extends Exception {

  public RequisitionException(String message) {
    super(message);
  }

  public RequisitionException(String message, Throwable cause) {
    super(message, cause);
  }
}
