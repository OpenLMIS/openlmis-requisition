package org.openlmis.errorhandling;

import org.openlmis.utils.ErrorResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Base classes for controller advices dealing with error handling.
 */
public abstract class AbstractErrorHandling {

  private final Logger logger = LoggerFactory.getLogger(getClass());

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
