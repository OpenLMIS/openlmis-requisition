package org.openlmis.requisition.exception;

/**
 * Signals user being unauthorized in external api.
 */
public class AuthorizationException extends Exception {
  public AuthorizationException(String message) {
    super(message);
  }
}
