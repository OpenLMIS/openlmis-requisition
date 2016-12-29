package org.openlmis.requisition.exception;

public class ReportingException extends BaseLocalizedException {
  public ReportingException(String messageKey, String... params) {
    super(messageKey, params);
  }

  public ReportingException(Throwable cause, String messageKey, String... params) {
    super(cause, messageKey, params);
  }
}
