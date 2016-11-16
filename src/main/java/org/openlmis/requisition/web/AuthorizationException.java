package org.openlmis.requisition.web;

/**
 * Signals user lacking permission to access the resource.
 */
public abstract class AuthorizationException extends Exception {
  public AuthorizationException(String message) {
    super(message);
  }
}
