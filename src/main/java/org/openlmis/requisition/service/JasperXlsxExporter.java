/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
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

import java.io.ByteArrayOutputStream;
import java.util.Arrays;
import java.util.List;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRPropertiesUtil;
import net.sf.jasperreports.engine.JRPropertiesUtil.PropertySuffix;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.export.ElementKeyExporterFilterFactory;
import net.sf.jasperreports.engine.export.JROriginExporterFilter;
import net.sf.jasperreports.engine.export.JRXlsAbstractExporter;
import net.sf.jasperreports.engine.export.ooxml.JRXlsxExporter;
import net.sf.jasperreports.export.SimpleExporterInput;
import net.sf.jasperreports.export.SimpleOutputStreamExporterOutput;

public class JasperXlsxExporter implements JasperExporter {

  private static final List<String> EXCLUSION_FILTERS = Arrays.asList(
      JROriginExporterFilter.PROPERTY_EXCLUDE_ORIGIN_PREFIX,
      ElementKeyExporterFilterFactory.PROPERTY_EXCLUDED_KEY_PREFIX);

  private JasperPrint jasperPrint;

  JasperXlsxExporter(JasperPrint jasperPrint) {
    this.jasperPrint = jasperPrint;
  }

  @Override
  public byte[] exportReport() throws JRException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    JRXlsxExporter exporter = new JRXlsxExporter();
    applyXlsExclusions(exporter.getExporterPropertiesPrefix());
    exporter.setExporterInput(new SimpleExporterInput(jasperPrint));
    exporter.setExporterOutput(new SimpleOutputStreamExporterOutput(baos));
    exporter.exportReport();
    return baos.toByteArray();
  }

  // JRXlsxExporter reads exclusion filters only under its own prefix, while templates
  // usually define them for XLS. A filter the template defines for XLSX is left as it is:
  // merging per key could build an origin out of band, group and report keys from both sets.
  private void applyXlsExclusions(String xlsxPrefix) {
    for (String filter : EXCLUSION_FILTERS) {
      if (!JRPropertiesUtil.getProperties(jasperPrint, xlsxPrefix + filter).isEmpty()) {
        continue;
      }
      String xlsFilterPrefix = JRXlsAbstractExporter.XLS_EXPORTER_PROPERTIES_PREFIX + filter;
      for (PropertySuffix property : JRPropertiesUtil.getProperties(jasperPrint, xlsFilterPrefix)) {
        jasperPrint.setProperty(xlsxPrefix + filter + property.getSuffix(), property.getValue());
      }
    }
  }
}
