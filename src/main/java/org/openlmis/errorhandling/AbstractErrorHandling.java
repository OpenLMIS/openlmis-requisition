package org.openlmis.errorhandling;

import org.openlmis.requisition.exception.AuthenticationException;
import org.openlmis.requisition.exception.PermissionException;
import org.openlmis.utils.ErrorResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

/**
 * Base classes for controller advices dealing with error handling.
 */
public abstract class AbstractErrorHandling {

  private final Logger logger = LoggerFactory.getLogger(getClass());

  @ExceptionHandler(AuthenticationException.class)
  @ResponseStatus(HttpStatus.UNAUTHORIZED)
  @ResponseBody
  public ErrorResponse handleAuthenticationException(AuthenticationException ex) {
    return logErrorAndRespond("Could not authenticate user", ex);
  }

  @ExceptionHandler(PermissionException.class)
  @ResponseStatus(HttpStatus.FORBIDDEN)
  @ResponseBody
  public ErrorResponse handlePermissionException(PermissionException ex) {
    return logErrorAndRespond("User is lacking permission to access the resource", ex);
  }

  /**
   * Logs an error message and returns an error response.
   * @param message the error message
   * @param ex the exception to log. Message from the exception is used as the
   *           error description.
   * @return the error response that should be sent to the client
   */
  protected ErrorResponse logErrorAndRespond(String message, Exception ex) {
    logger.error(message, ex);
    return new ErrorResponse(message, ex.getMessage());
  }
}
