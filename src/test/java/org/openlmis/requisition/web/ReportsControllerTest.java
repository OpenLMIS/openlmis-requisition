package org.openlmis.requisition.web;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperReport;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openlmis.requisition.domain.JasperTemplate;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.JasperReportViewException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.JasperReportsViewService;
import org.openlmis.requisition.service.JasperTemplateService;
import org.openlmis.requisition.service.PermissionService;
import org.springframework.core.io.ClassPathResource;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.view.jasperreports.JasperReportsMultiFormatView;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;

@SuppressWarnings({"PMD.UnusedPrivateField"})
public class ReportsControllerTest {
  private static final String REQUISITION_TEMPLATE_PATH = "jasperTemplates/requisition.jrxml";

  @Mock
  private RequisitionRepository requisitionRepository;

  @Mock
  private PermissionService permissionService;

  @Mock
  private JasperTemplateService jasperTemplateService;

  @Mock
  private JasperReportsViewService jasperReportsViewService;

  @InjectMocks
  private ReportsController reportsController;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test(expected = ContentNotFoundMessageException.class)
  public void shouldNotPrintRequisitionIfTRequisitionDoesNotExist()
      throws JasperReportViewException {
    // when
    reportsController.print(mock(HttpServletRequest.class), UUID.randomUUID());
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldNotPrintRequisitionIfTemplateDoesNotExist()
      throws JasperReportViewException {
    // given
    when(requisitionRepository.findOne(any(UUID.class))).thenReturn(mock(Requisition.class));

    // when
    reportsController.print(mock(HttpServletRequest.class), UUID.randomUUID());
  }

  @Test
  public void shouldPrintRequisition()
      throws JasperReportViewException, IOException, JRException {
    // given
    JasperTemplate template = mock(JasperTemplate.class);
    HttpServletRequest request = mock(HttpServletRequest.class);
    byte[] data = getTemplateData(REQUISITION_TEMPLATE_PATH);

    when(requisitionRepository.findOne(any(UUID.class))).thenReturn(mock(Requisition.class));
    when(jasperReportsViewService.getJasperReportsView(template, request))
        .thenReturn(new JasperReportsMultiFormatView());
    when(jasperTemplateService.getByName(any(String.class))).thenReturn(template);
    when(template.getData()).thenReturn(data);

    // when
    ModelAndView view = reportsController.print(request, UUID.randomUUID());

    // then
    assertTrue(view.getView() instanceof JasperReportsMultiFormatView);
  }

  private byte[] getTemplateData(String path) throws IOException, JRException {
    ClassPathResource resource = new ClassPathResource(path);
    JasperReport report = JasperCompileManager.compileReport(resource.getInputStream());

    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    ObjectOutputStream out = new ObjectOutputStream(bos);
    out.writeObject(report);

    return bos.toByteArray();
  }
}
