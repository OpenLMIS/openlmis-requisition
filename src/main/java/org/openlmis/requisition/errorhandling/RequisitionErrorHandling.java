package org.openlmis.requisition.errorhandling;

import org.openlmis.errorhandling.AbstractErrorHandling;
import org.openlmis.requisition.exception.CommentNotFoundException;
import org.openlmis.utils.ErrorResponse;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

/**
 * Error handling for requisition related exceptions.
 */
@ControllerAdvice
public class RequisitionErrorHandling extends AbstractErrorHandling {

  @ExceptionHandler(CommentNotFoundException.class)
  @ResponseStatus(HttpStatus.NOT_FOUND)
  @ResponseBody
  public ErrorResponse handleCommentNotFoundException(CommentNotFoundException ex) {
    return logErrorAndRespond("Comment not found", ex);
  }
}
