package org.openlmis.reporting.errorhandling;

import org.openlmis.errorhandling.AbstractErrorHandling;
import org.openlmis.reporting.exception.ReportingException;
import org.openlmis.utils.ErrorResponse;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;

/**
 * Controller advice for handling reporting errors.
 */
@ControllerAdvice
public class ReportingErrorHandling extends AbstractErrorHandling {

  @ExceptionHandler(ReportingException.class)
  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  public ErrorResponse handlerReportingException(ReportingException ex) {
    return logErrorAndRespond("Reporting error", ex);
  }
}
