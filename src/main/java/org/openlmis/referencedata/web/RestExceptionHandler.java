package org.openlmis.referencedata.web;

import org.openlmis.referencedata.exception.CsvInputNotValidException;
import org.openlmis.referencedata.exception.ExceptionDetail;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import java.util.Date;

import javax.servlet.http.HttpServletRequest;

@ControllerAdvice
public class RestExceptionHandler {

  /**
   * Handle given exception and returns proper response.
   *
   * @param ex Exception to handle.
   * @param request Request to handle.
   * @return ResponseEntity with exception details
   */
  @ExceptionHandler(CsvInputNotValidException.class)
  public ResponseEntity<ExceptionDetail> csvInputNotValidExceptionHandler(
      RuntimeException ex, 
      HttpServletRequest request) {
    HttpStatus status = HttpStatus.BAD_REQUEST;
    String title = "Resource Property Validation Failure";
    ExceptionDetail exceptionDetail = getExceptionDetail(ex, status, title);
    return new ResponseEntity<ExceptionDetail>(exceptionDetail, null, status);
  }

  private static ExceptionDetail getExceptionDetail(
          Exception exception, HttpStatus status, String title) {
    ExceptionDetail exceptionDetail = new ExceptionDetail();
    exceptionDetail.setTimeStamp(new Date().getTime());
    exceptionDetail.setStatus(status.value());
    exceptionDetail.setTitle(title);
    exceptionDetail.setDetail(exception.getMessage());
    exceptionDetail.setDeveloperMessage(exception.getClass().getName());
    return exceptionDetail;
  }
}