package org.openlmis.requisition.exception;

import java.util.Map;

/**
 * Exception thrown if there were any validation errors.
 */
public class BindingResultException extends RuntimeException {

  private final Map<String, String> errors;

  public BindingResultException(Map<String, String> errors) {
    this.errors = errors;
  }

  public Map<String, String> getErrors() {
    return errors;
  }
}
