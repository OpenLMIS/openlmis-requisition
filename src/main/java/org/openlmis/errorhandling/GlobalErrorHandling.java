package org.openlmis.errorhandling;

import org.openlmis.requisition.exception.AuthenticationMessageException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.service.referencedata.ReferenceDataRetrievalException;
import org.openlmis.requisition.web.PermissionMessageException;
import org.openlmis.util.ErrorResponse;
import org.openlmis.utils.Message;
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
public class GlobalErrorHandling extends AbstractErrorHandling {
  
  @ExceptionHandler(ReferenceDataRetrievalException.class)
  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ResponseBody
  public ErrorResponse handleRefDataException(ReferenceDataRetrievalException ex) {
    return logErrorAndRespond("Error fetching from reference data", ex);
  }

  /**
   * Handles Message exceptions and returns status 400 Bad Request.
   *
   * @param ex the ValidationMessageException to handle
   * @return the error response for the user
   */
  @ExceptionHandler(ValidationMessageException.class)
  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ResponseBody
  public Message.LocalizedMessage handleMessageException(ValidationMessageException ex) {
    return getLocalizedMessage(ex);
  }

  @ExceptionHandler(AuthenticationMessageException.class)
  @ResponseStatus(HttpStatus.UNAUTHORIZED)
  @ResponseBody
  public Message.LocalizedMessage handleAuthenticationException(AuthenticationMessageException ex) {
    return getLocalizedMessage(ex);
  }

  @ExceptionHandler(PermissionMessageException.class)
  @ResponseStatus(HttpStatus.FORBIDDEN)
  @ResponseBody
  public Message.LocalizedMessage handlePermissionException(PermissionMessageException ex) {
    return getLocalizedMessage(ex);
  }
}
