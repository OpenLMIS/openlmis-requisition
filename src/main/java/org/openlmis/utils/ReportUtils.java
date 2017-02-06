package org.openlmis.utils;

import static net.sf.jasperreports.engine.JRParameter.REPORT_LOCALE;
import static net.sf.jasperreports.engine.JRParameter.REPORT_RESOURCE_BUNDLE;

import net.sf.jasperreports.engine.JRBand;
import net.sf.jasperreports.engine.JRChild;
import net.sf.jasperreports.engine.design.JRDesignTextField;

import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.springframework.context.i18n.LocaleContextHolder;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.stream.Collectors;

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

    return params;
  }

  /**
   * Sorts the map of requisition template columns by their display order, without 'skipped' column.
   * @param map map of column keys to columns.
   * @return sorted map.
   */
  public static LinkedHashMap<String, RequisitionTemplateColumn>
      getSortedTemplateColumnsForPrint(Map<String, RequisitionTemplateColumn> map) {
    List<Map.Entry<String, RequisitionTemplateColumn>> sorted = map.entrySet().stream()
        .filter(ent -> !ent.getKey().equals("skipped"))
        .sorted(Comparator.comparingInt(ent -> ent.getValue().getDisplayOrder()))
        .collect(Collectors.toList());

    LinkedHashMap<String, RequisitionTemplateColumn> result = new LinkedHashMap<>();
    for (Map.Entry<String, RequisitionTemplateColumn> entry : sorted) {
      result.put(entry.getKey(), entry.getValue());
    }

    return result;
  }

  /**
   * Customizes template band to adjust columns order.
   * @param band Jasper Report band to edit.
   * @param columns map of requisition template columns.
   * @param margin page margin, to adjust initial column positions.
   */
  public static void customizeBandWithTemplateFields(
      JRBand band, Map<String, RequisitionTemplateColumn> columns, int margin) {
    List<JRDesignTextField> foundFields = new ArrayList<>();

    JRDesignTextField prevField = null;
    for (Map.Entry<String, RequisitionTemplateColumn> entry : columns.entrySet()) {
      String fieldName = entry.getKey();
      JRDesignTextField field = (JRDesignTextField)band.getElementByKey(fieldName);

      if (field != null) {
        setPositionAfterPreviousField(field, prevField, margin);
        foundFields.add(field);
        prevField = field;
      }
    }

    for (JRChild child : band.getChildren()) {
      if (child instanceof JRDesignTextField && !foundFields.contains(child)) {
        JRDesignTextField field = (JRDesignTextField)child;
        setPositionAfterPreviousField(field, prevField, margin);
        prevField = field;
      }
    }
  }

  private static void setPositionAfterPreviousField(
      JRDesignTextField field, JRDesignTextField prev, int margin) {
    if (prev == null) {
      field.setX(margin);
    } else {
      field.setX(prev.getX() + prev.getWidth());
    }
  }
}
