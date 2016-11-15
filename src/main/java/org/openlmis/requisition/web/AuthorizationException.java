package org.openlmis.requisition.web;

/**
 * Signals user lacking permission to access the resource.
 */
public abstract class AuthorizationException extends RuntimeException {
  public AuthorizationException(String message) {
    super(message);
  }
}
