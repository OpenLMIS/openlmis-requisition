package org.openlmis.referencedata.web;


import org.openlmis.referencedata.exception.ExceptionDetail;
import org.openlmis.referencedata.exception.EmptyObjectException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import javax.servlet.http.HttpServletRequest;
import java.util.Date;

@ControllerAdvice
public class RestExceptionHandler {
  
  @ExceptionHandler(EmptyObjectException.class)
  public ResponseEntity<ExceptionDetail> emptyObjectExceptionHandler(RuntimeException ex, HttpServletRequest request) {
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