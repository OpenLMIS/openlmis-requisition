package org.openlmis.errorhandling;

import org.openlmis.utils.ErrorResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

/**
 * Global error handling for all controllers in the service.
 * Contains common error handling mappings.
 */
@ControllerAdvice
public class GlobalErrorHandling {

  /**
   * Handles the {@link DataIntegrityViolationException} which signals a violation of some sort
   * of a db constraint like unique. Returns error 409 (CONFLICT) and a JSON representation of the
   * error as the body.
   * @param ex the exception that caused the issue
   * @return the error response
   */
  @ExceptionHandler(DataIntegrityViolationException.class)
  @ResponseStatus(HttpStatus.CONFLICT)
  @ResponseBody
  public ErrorResponse handleDataIntegrityViolation(DataIntegrityViolationException ex) {
    final String msg = "Data integrity violation";
    LOGGER.error(msg, ex);
    return new ErrorResponse(msg, ex.getMessage());
  }

  private static final Logger LOGGER = LoggerFactory.getLogger(GlobalErrorHandling.class);
}
