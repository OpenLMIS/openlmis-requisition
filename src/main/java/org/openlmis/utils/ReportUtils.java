package org.openlmis.utils;

import static net.sf.jasperreports.engine.JRParameter.REPORT_LOCALE;
import static net.sf.jasperreports.engine.JRParameter.REPORT_RESOURCE_BUNDLE;

import org.springframework.context.i18n.LocaleContextHolder;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;

public final class ReportUtils {

  private ReportUtils() {
    throw new UnsupportedOperationException();
  }

  /**
   * Set parameters of rendered pdf report.
   */
  public static Map<String, Object> createParametersMap() {
    Map<String, Object> params = new HashMap<>();
    params.put("format", "pdf");

    Locale currentLocale = LocaleContextHolder.getLocale();
    params.put(REPORT_LOCALE, currentLocale);

    ResourceBundle resourceBundle = ResourceBundle.getBundle("messages", currentLocale);
    params.put(REPORT_RESOURCE_BUNDLE, resourceBundle);

    params.put("subreport_dir", "reports/");
    params.put("image_dir", "images/");

    return params;
  }
}
