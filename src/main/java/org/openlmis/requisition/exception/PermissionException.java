package org.openlmis.requisition.exception;

/**
 * Signals user lacking permission to access the resource.
 */
public class PermissionException extends RuntimeException {
  public PermissionException(String message) {
    super(message);
  }
}
