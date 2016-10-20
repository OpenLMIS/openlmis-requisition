package org.openlmis.requisition.errorhandling;

import org.openlmis.errorhandling.AbstractErrorHandling;
import org.openlmis.requisition.exception.CommentNotFoundException;
import org.openlmis.requisition.exception.IdMismatchException;
import org.openlmis.requisition.exception.InvalidRequisitionStatusException;
import org.openlmis.requisition.exception.RequisitionAlreadyExistsException;
import org.openlmis.requisition.exception.RequisitionException;
import org.openlmis.requisition.exception.RequisitionLineItemNotFoundException;
import org.openlmis.requisition.exception.RequisitionNotFoundException;
import org.openlmis.requisition.exception.SkipNotAllowedException;
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

  @ExceptionHandler({InvalidRequisitionStatusException.class,
          RequisitionAlreadyExistsException.class,
          SkipNotAllowedException.class})
  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ResponseBody
  public ErrorResponse handleBadRequisitionRequest(
          RequisitionException ex) {
    return logErrorAndRespond("Operation cannot be executed on requisition", ex);
  }

  @ExceptionHandler({RequisitionNotFoundException.class,
          RequisitionLineItemNotFoundException.class})
  @ResponseStatus(HttpStatus.NOT_FOUND)
  @ResponseBody
  public ErrorResponse handleRequisitionOrItemNotFound(
          RequisitionException ex) {
    return logErrorAndRespond("Requisition not found.", ex);
  }

  @ExceptionHandler({IdMismatchException.class})
  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ResponseBody
  public ErrorResponse handleIdMismatchException(
      IdMismatchException ex) {
    return logErrorAndRespond("Requisition id mismatch.", ex);
  }
}
