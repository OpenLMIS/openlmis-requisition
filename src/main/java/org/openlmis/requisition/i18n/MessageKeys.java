package org.openlmis.requisition.i18n;

public abstract class MessageKeys {
  private static final String SERVICE_PREFIX = "requisition";
  private static final String ERROR_PREFIX = SERVICE_PREFIX + ".error";

  public static final String ERROR_CLASS_NOT_FOUND = ERROR_PREFIX + ".class-not-found";
  public static final String ERROR_IO = ERROR_PREFIX + ".io";

  public static final String ERROR_JASPER_FILE_CREATION = ERROR_PREFIX + ".jasper.file-creation";

  public static final String ERROR_REPORTING_CREATION = ERROR_PREFIX + ".reporting.creation";
  public static final String ERROR_REPORTING_EXTRA_PROPERTIES = ERROR_PREFIX
      + ".reporting.extra-properties";
  public static final String ERROR_REPORTING_FILE_EMPTY = ERROR_PREFIX + ".reporting.file.empty";
  public static final String ERROR_REPORTING_FILE_INCORRECT_TYPE = ERROR_PREFIX
      + ".reporting.file.incorrect-type";
  public static final String ERROR_REPORTING_FILE_INVALID = ERROR_PREFIX
      + ".reporting.file.invalid";
  public static final String ERROR_REPORTING_FILE_MISSING = ERROR_PREFIX
      + ".reporting.file.missing";
  public static final String ERROR_REPORTING_PARAMETER_INCORRECT_TYPE = ERROR_PREFIX
      + ".reporting.parameter.incorrect-type";
  public static final String ERROR_REPORTING_PARAMETER_MISSING = ERROR_PREFIX
      + ".reporting.parameter.missing";
  public static final String ERROR_REPORTING_TEMPLATE_EXIST = ERROR_PREFIX
      + ".reporting.template.exist";

  private MessageKeys() {
    throw new UnsupportedOperationException();
  }
}
