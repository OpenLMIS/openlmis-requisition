package org.openlmis.requisition.exception;


public class CannotChangeFieldException extends RequisitionException {
  public CannotChangeFieldException(String message) {
    super("Field " + message + " cannot be changed.");
  }
}
