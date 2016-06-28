package org.openlmis.referencedata.web;

import java.util.Date;
import javax.servlet.http.HttpServletRequest;

import org.openlmis.referencedata.exception.ExceptionDetail;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class RestExceptionHandler
{
    @ExceptionHandler(javax.validation.ConstraintViolationException.class)
    public ResponseEntity<?> handleConstraintViolationException(javax.validation.ConstraintViolationException exception, HttpServletRequest request)
    {
        HttpStatus status = HttpStatus.BAD_REQUEST;
        String title = "Resource Property Validation Failure";
        ExceptionDetail exceptionDetail = getExceptionDetail(exception, status, title);

        return new ResponseEntity<>(exceptionDetail, null, status);
    }

    //TODO: Determine why this ExceptionHandler isn't being used
    @ExceptionHandler(org.hibernate.exception.ConstraintViolationException.class)
    public ResponseEntity<?> handleConstraintViolationException2(org.hibernate.exception.ConstraintViolationException exception, HttpServletRequest request)
    {
        HttpStatus status = HttpStatus.BAD_REQUEST;
        String title = "Resource Property Validation Failure";
        ExceptionDetail exceptionDetail = getExceptionDetail(exception, status, title);

        return new ResponseEntity<>(exceptionDetail, null, status);
    }

    private static ExceptionDetail getExceptionDetail(Exception exception, HttpStatus status, String title)
    {
        ExceptionDetail exceptionDetail = new ExceptionDetail();
        exceptionDetail.setTimeStamp(new Date().getTime());
        exceptionDetail.setStatus(status.value());
        exceptionDetail.setTitle(title);
        exceptionDetail.setDetail(exception.getMessage());
        exceptionDetail.setDeveloperMessage(exception.getClass().getName());
        return exceptionDetail;
    }
}