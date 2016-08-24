package org.openlmis.reporting;

import static net.sf.jasperreports.engine.export.JRHtmlExporterParameter.IS_USING_IMAGES_TO_ALIGN;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.spy;
import static org.powermock.api.mockito.PowerMockito.whenNew;

import net.sf.jasperreports.engine.JRExporterParameter;
import net.sf.jasperreports.engine.JasperReport;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.openlmis.reporting.model.Template;
import org.openlmis.reporting.service.JasperReportsViewFactory;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.modules.junit4.PowerMockRunnerDelegate;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockServletContext;
import org.springframework.web.servlet.view.jasperreports.JasperReportsMultiFormatView;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.sql.DataSource;

@RunWith(PowerMockRunner.class)
@PowerMockRunnerDelegate(BlockJUnit4ClassRunner.class)
@PrepareForTest({JasperReportsViewFactory.class})
public class JasperReportsViewFactoryTest {

  @Mock
  private DataSource dataSource;

  @InjectMocks
  private JasperReportsViewFactory viewFactory;

  private Template template;
  private JasperReportsMultiFormatView jasperReportsView;
  private JasperReport jasperReport;
  private ObjectInputStream objectInputStream;
  private ObjectOutputStream objectOutputStream;
  private ByteArrayOutputStream byteArrayOutputStream;
  private byte[] reportByteData;

  @Before
  public void setUp() throws Exception {
    template = mock(Template.class);
    when(template.getName()).thenReturn("report1.jrxml");
    reportByteData = new byte[1];
    when(template.getData()).thenReturn(reportByteData);
    jasperReport = mock(JasperReport.class);

    objectInputStream = mock(ObjectInputStream.class);
    objectOutputStream = mock(ObjectOutputStream.class);
    byteArrayOutputStream = mock(ByteArrayOutputStream.class);

    ByteArrayInputStream byteArrayInputStream = mock(ByteArrayInputStream.class);
    whenNew(ByteArrayInputStream.class).withArguments(reportByteData)
        .thenReturn(byteArrayInputStream);
    whenNew(ObjectInputStream.class).withArguments(byteArrayInputStream)
        .thenReturn(objectInputStream);
    whenNew(ByteArrayOutputStream.class).withNoArguments().thenReturn(byteArrayOutputStream);
    whenNew(ObjectOutputStream.class).withArguments(byteArrayOutputStream)
        .thenReturn(objectOutputStream);
    jasperReportsView = spy(new JasperReportsMultiFormatView());
  }

  @Test
  public void testShouldGetRequestedViewAndSetDataSourceAndWebContextInJasperView()
      throws Exception {
    whenNew(JasperReportsMultiFormatView.class).withNoArguments().thenReturn(jasperReportsView);
    when(objectInputStream.readObject()).thenReturn(jasperReport);
    when(byteArrayOutputStream.toByteArray()).thenReturn(reportByteData);

    ServletContext servletContext = new MockServletContext("");
    HttpServletRequest httpServletRequest = new MockHttpServletRequest(servletContext);
    JasperReportsMultiFormatView reportView = viewFactory.getJasperReportsView(
        template, httpServletRequest);

    assertThat(reportView, is(jasperReportsView));
    verify(jasperReportsView).setJdbcDataSource(dataSource);
    verify(objectOutputStream).writeObject(jasperReport);
  }

  @Test
  public void testShouldAddExportParamToGetRidOfImageInHtmlReport() throws Exception {
    whenNew(JasperReportsMultiFormatView.class).withNoArguments().thenReturn(jasperReportsView);
    when(objectInputStream.readObject()).thenReturn(jasperReport);
    when(byteArrayOutputStream.toByteArray()).thenReturn(reportByteData);

    Map<JRExporterParameter, Object> exportParams = new HashMap<>();
    exportParams.put(IS_USING_IMAGES_TO_ALIGN, false);
    ServletContext servletContext = new MockServletContext("");
    HttpServletRequest httpServletRequest = new MockHttpServletRequest(servletContext);
    viewFactory.getJasperReportsView(template, httpServletRequest);

    verify(jasperReportsView).setExporterParameters(exportParams);
  }
}
