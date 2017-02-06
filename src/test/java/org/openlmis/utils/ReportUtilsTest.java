package org.openlmis.utils;

import static net.sf.jasperreports.engine.JRParameter.REPORT_LOCALE;
import static net.sf.jasperreports.engine.JRParameter.REPORT_RESOURCE_BUNDLE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.springframework.context.i18n.LocaleContextHolder;

import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.stream.Collectors;

public class ReportUtilsTest {
  @Test
  public void shouldCreateDefaultParametersMap() {
    // when
    Map<String, Object> map = ReportUtils.createParametersMap();

    // then
    String formatParam = "format";

    assertTrue(map.containsKey(formatParam));
    assertEquals(map.get(formatParam), "pdf");

    Locale currentLocale = LocaleContextHolder.getLocale();
    assertTrue(map.containsKey(REPORT_LOCALE));
    assertEquals(map.get(REPORT_LOCALE), currentLocale);

    ResourceBundle resourceBundle = ResourceBundle.getBundle("messages", currentLocale);
    assertTrue(map.containsKey(REPORT_RESOURCE_BUNDLE));
    assertEquals(map.get(REPORT_RESOURCE_BUNDLE), resourceBundle);
  }

  @Test
  public void shouldReturnSortedColumnsMap() {
    // given
    Map<String, RequisitionTemplateColumn> map = new HashMap<>();

    RequisitionTemplateColumn firstColumn = mock(RequisitionTemplateColumn.class);
    when(firstColumn.getDisplayOrder()).thenReturn(1);
    map.put("first", firstColumn);

    RequisitionTemplateColumn secondColumn = mock(RequisitionTemplateColumn.class);
    when(secondColumn.getDisplayOrder()).thenReturn(5);
    map.put("second", secondColumn);

    RequisitionTemplateColumn thirdColumn = mock(RequisitionTemplateColumn.class);
    when(thirdColumn.getDisplayOrder()).thenReturn(10);
    map.put("third", thirdColumn);

    // when
    Map<String, RequisitionTemplateColumn> result =
        ReportUtils.getSortedTemplateColumnsForPrint(map);

    // then
    assertEquals(result.size(), map.size());

    List<RequisitionTemplateColumn> columns = result.values().stream().collect(Collectors.toList());
    assertEquals(columns.get(0), firstColumn);
    assertEquals(columns.get(1), secondColumn);
    assertEquals(columns.get(2), thirdColumn);
  }

  @Test
  public void shouldFilterOutSkippedColumn() {
    // given
    Map<String, RequisitionTemplateColumn> map = new HashMap<>();
    map.put("skipped", mock(RequisitionTemplateColumn.class));

    // when
    Map<String, RequisitionTemplateColumn> result =
        ReportUtils.getSortedTemplateColumnsForPrint(map);

    // then
    assertTrue(result.isEmpty());
  }
}
