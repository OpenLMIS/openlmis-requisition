package org.openlmis.referencedata.web;

import org.openlmis.referencedata.exception.CsvInputNotValidException;
import org.openlmis.referencedata.exception.ExceptionDetail;
import org.openlmis.requisition.exception.RequisitionNotFoundException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import java.time.ZonedDateTime;

@ControllerAdvice
public class RestExceptionHandler {

  private static final Logger LOGGER = LoggerFactory.getLogger(
          RestExceptionHandler.class);

  /**
   * Handle given exception and returns proper response.
   *
   * @param ex Exception to handle.
   * @return ResponseEntity with exception details
   */
  @ExceptionHandler(CsvInputNotValidException.class)
  public ResponseEntity<ExceptionDetail> csvInputNotValidExceptionHandler(
          HttpRequest request, RuntimeException ex) {
    logException(ex, request);
    HttpStatus status = HttpStatus.BAD_REQUEST;
    String title = "Resource Property Validation Failure";
    ExceptionDetail exceptionDetail = getExceptionDetail(ex, status, title);
    return new ResponseEntity<>(exceptionDetail, null, status);
  }

  /**
   * Handles requisition not found exception - status 400.
   * @param ex the exception to handle
   * @return the error
   */
  @ExceptionHandler(RequisitionNotFoundException.class)
  public ResponseEntity<ExceptionDetail> handleReqNotFoundException(
          HttpRequest request, RequisitionNotFoundException ex) {
    logException(ex, request);
    ExceptionDetail detail = getExceptionDetail(ex, HttpStatus.BAD_REQUEST,
            "Requisition not found");
    return new ResponseEntity<>(detail, HttpStatus.BAD_REQUEST);
  }

  private static ExceptionDetail getExceptionDetail(
          Exception exception, HttpStatus status, String title) {
    ExceptionDetail exceptionDetail = new ExceptionDetail();
    exceptionDetail.setTimeStamp(ZonedDateTime.now().toInstant().toEpochMilli());
    exceptionDetail.setStatus(status.value());
    exceptionDetail.setTitle(title);
    exceptionDetail.setDetail(exception.getMessage());
    exceptionDetail.setDeveloperMessage(exception.getClass().getName());
    return exceptionDetail;
  }

  private void logException(Exception ex, HttpRequest request) {
    LOGGER.error("Error while executing request: {}", request.getURI(), ex);
  }
}