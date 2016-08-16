package org.openlmis.fulfillment.utils;

import net.sf.jasperreports.engine.JRParameter;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;

import java.io.IOException;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;

public class ReportUtils {

  private static final String REPORTS_RESOURCE = "reports";

  private static final String IMAGES_RESOURCE = "images";

  /**
   * Set parameters of rendered pdf report.
   */
  public static Map<String, Object> createParametersMap() throws IOException {
    Map<String, Object> params = new HashMap<>();
    params.put("format", "pdf");

    Locale currentLocale = LocaleContextHolder.getLocale();
    params.put(JRParameter.REPORT_LOCALE, currentLocale);
    ResourceBundle resourceBundle = ResourceBundle.getBundle("messages", currentLocale);
    params.put(JRParameter.REPORT_RESOURCE_BUNDLE, resourceBundle);

    Resource reportResource = new ClassPathResource(REPORTS_RESOURCE);
    Resource imgResource = new ClassPathResource(IMAGES_RESOURCE);
    String separator = System.getProperty("file.separator");
    params.put("subreport_dir", reportResource.getFile().getAbsolutePath() + separator);
    params.put("image_dir", imgResource.getFile().getAbsolutePath() + separator);
    return params;
  }
}
