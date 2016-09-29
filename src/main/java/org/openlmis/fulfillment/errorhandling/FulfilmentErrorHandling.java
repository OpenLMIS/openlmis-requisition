package org.openlmis.fulfillment.errorhandling;

import org.openlmis.errorhandling.AbstractErrorHandling;
import org.openlmis.fulfillment.exception.OrderFileException;
import org.openlmis.utils.ErrorResponse;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

/**
 * Controller advice responsible for handling errors specific to fulfilment.
 */
@ControllerAdvice
public class FulfilmentErrorHandling extends AbstractErrorHandling {

  @ExceptionHandler(OrderFileException.class)
  public ErrorResponse handleOrderFileGenerationError(OrderFileException ex) {
    return logErrorAndRespond("Unable to generate the order file", ex);
  }
}
