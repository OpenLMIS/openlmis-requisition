package org.openlmis.reporting.service;

import static java.io.File.createTempFile;
import static net.sf.jasperreports.engine.export.JRHtmlExporterParameter.IS_USING_IMAGES_TO_ALIGN;
import static org.apache.commons.io.FileUtils.writeByteArrayToFile;

import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRExporterParameter;
import net.sf.jasperreports.engine.JasperReport;
import org.openlmis.reporting.model.Template;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.support.WebApplicationContextUtils;
import org.springframework.web.servlet.view.jasperreports.JasperReportsMultiFormatView;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.sql.DataSource;

@Service
public class JasperReportsViewFactory {

  @Autowired
  private DataSource replicationDataSource;

  /**
   *  Create Jasper Report View.
   *  Create Jasper Report (".jasper" file) from bytes from Template entity.
   *  Set 'Jasper' exporter parameters, data source, web application context, url to file.
   */
  public JasperReportsMultiFormatView getJasperReportsView(Template template,
                                                           HttpServletRequest request)
      throws IOException, ClassNotFoundException, JRException {
    JasperReportsMultiFormatView jasperView = new JasperReportsMultiFormatView();

    setExportParams(jasperView);
    setDataSourceAndUrlAndApplicationContext(template, jasperView, request);
    return jasperView;
  }

  /**
   * Set exporter parameters in jasper view.
   */
  private void setExportParams(JasperReportsMultiFormatView jasperView) {
    Map<JRExporterParameter, Object> reportFormatMap = new HashMap<>();
    reportFormatMap.put(IS_USING_IMAGES_TO_ALIGN, false);
    jasperView.setExporterParameters(reportFormatMap);
  }

  /**
   * Set application context, data source and url to file in jasper view.
   */
  private void setDataSourceAndUrlAndApplicationContext(Template template,
                                                        JasperReportsMultiFormatView jasperView,
                                                        HttpServletRequest servletRequest)
      throws IOException, ClassNotFoundException, JRException {

    ServletContext servletContext = servletRequest.getSession().getServletContext();
    WebApplicationContext ctx =
        WebApplicationContextUtils.getWebApplicationContext(servletContext);
    jasperView.setJdbcDataSource(replicationDataSource);
    jasperView.setUrl(getReportUrlForReportData(template));
    if (ctx != null) {
      jasperView.setApplicationContext(ctx);
    }
  }

  /**
   * Create ".jasper" file with byte array from Template.
   * @return Url to ".jasper" file.
   */
  private String getReportUrlForReportData(Template template)
      throws IOException, ClassNotFoundException, JRException {

    File tmpFile = createTempFile(template.getName() + "_temp", ".jasper");
    ObjectInputStream inputStream = new ObjectInputStream(
        new ByteArrayInputStream(template.getData()));
    JasperReport jasperReport = (JasperReport) inputStream.readObject();
    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    ObjectOutputStream out = new ObjectOutputStream(bos);
    out.writeObject(jasperReport);
    writeByteArrayToFile(tmpFile, bos.toByteArray());
    return tmpFile.toURI().toURL().toString();
  }
}
