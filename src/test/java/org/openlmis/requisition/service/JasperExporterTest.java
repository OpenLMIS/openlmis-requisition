/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2020 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org.
 */

package org.openlmis.requisition.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JROrigin;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.base.JRBasePrintPage;
import net.sf.jasperreports.engine.base.JRBasePrintText;
import net.sf.jasperreports.engine.type.BandTypeEnum;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;

public class JasperExporterTest {

  private static final String HEADER = "Facility Name";
  private static final String FOOTER = "Page footer";

  @Test
  public void csvExportReportShouldReturnData() throws JRException {
    JasperCsvExporter exporter = new JasperCsvExporter(mock(JasperPrint.class));
    assertNotNull(exporter.exportReport());
  }

  @Test
  public void xlsExportReportShouldReturnData() throws JRException {
    JasperXlsExporter exporter = new JasperXlsExporter(new JasperPrint());
    assertNotNull(exporter.exportReport());
  }

  @Test
  public void xlsxExportReportShouldReturnData() throws JRException {
    JasperXlsxExporter exporter = new JasperXlsxExporter(new JasperPrint());
    assertNotNull(exporter.exportReport());
  }

  @Test
  public void xlsxExportReportShouldApplyXlsBandExclusions() throws JRException, IOException {
    JasperPrint jasperPrint = twoPagePrint();
    jasperPrint.setProperty(
        "net.sf.jasperreports.export.xls.exclude.origin.band.1", "columnHeader");
    jasperPrint.setProperty(
        "net.sf.jasperreports.export.xls.exclude.origin.band.2", "pageFooter");
    jasperPrint.setProperty(
        "net.sf.jasperreports.export.xls.exclude.origin.keep.first.band.1", "columnHeader");

    String sheet = sheetXml(new JasperXlsxExporter(jasperPrint).exportReport());

    assertEquals(1, StringUtils.countMatches(sheet, HEADER));
    assertEquals(0, StringUtils.countMatches(sheet, FOOTER));
  }

  @Test
  public void xlsxExportReportShouldApplyXlsKeyExclusions() throws JRException, IOException {
    JasperPrint jasperPrint = twoPagePrint();
    jasperPrint.setProperty("net.sf.jasperreports.export.xls.exclude.key.1", "footer");

    String sheet = sheetXml(new JasperXlsxExporter(jasperPrint).exportReport());

    assertEquals(2, StringUtils.countMatches(sheet, HEADER));
    assertEquals(0, StringUtils.countMatches(sheet, FOOTER));
  }

  @Test
  public void xlsxExportReportShouldPreferXlsxBandExclusions() throws JRException, IOException {
    JasperPrint jasperPrint = twoPagePrint();
    jasperPrint.setProperty(
        "net.sf.jasperreports.export.xls.exclude.origin.band.header", "columnHeader");
    jasperPrint.setProperty(
        "net.sf.jasperreports.export.xlsx.exclude.origin.band.footer", "pageFooter");

    String sheet = sheetXml(new JasperXlsxExporter(jasperPrint).exportReport());

    assertEquals(2, StringUtils.countMatches(sheet, HEADER));
    assertEquals(0, StringUtils.countMatches(sheet, FOOTER));
  }

  @Test
  public void htmlExportReportShouldReturnData() throws JRException {
    JasperHtmlExporter exporter = new JasperHtmlExporter(mock(JasperPrint.class));
    assertNotNull(exporter.exportReport());
  }

  private JasperPrint twoPagePrint() {
    JasperPrint jasperPrint = new JasperPrint();
    jasperPrint.setPageWidth(200);
    jasperPrint.setPageHeight(100);
    for (int page = 0; page < 2; page++) {
      JRBasePrintPage printPage = new JRBasePrintPage();
      printPage.addElement(text(jasperPrint, "header", HEADER, BandTypeEnum.COLUMN_HEADER, 1, 0));
      printPage.addElement(text(jasperPrint, "footer", FOOTER, BandTypeEnum.PAGE_FOOTER, 2, 80));
      jasperPrint.addPage(printPage);
    }
    return jasperPrint;
  }

  private JRBasePrintText text(JasperPrint jasperPrint, String key, String value,
      BandTypeEnum band, int sourceElementId, int y) {
    JRBasePrintText text = new JRBasePrintText(jasperPrint.getDefaultStyleProvider());
    text.setText(value);
    text.setKey(key);
    text.setOrigin(new JROrigin(band));
    text.setSourceElementId(sourceElementId);
    text.setY(y);
    text.setWidth(200);
    text.setHeight(20);
    return text;
  }

  private String sheetXml(byte[] workbook) throws IOException {
    try (ZipInputStream zip = new ZipInputStream(new ByteArrayInputStream(workbook))) {
      for (ZipEntry entry = zip.getNextEntry(); entry != null; entry = zip.getNextEntry()) {
        if ("xl/worksheets/sheet1.xml".equals(entry.getName())) {
          return IOUtils.toString(zip, StandardCharsets.UTF_8);
        }
      }
    }
    throw new AssertionError("The workbook has no first sheet");
  }
}
