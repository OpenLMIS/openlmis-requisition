package org.openlmis.requisition.web;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import net.sf.jasperreports.engine.JRException;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.JasperReportViewException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.JasperReportsViewService;
import org.openlmis.requisition.service.JasperTemplateService;
import org.openlmis.requisition.service.PermissionService;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
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
  private RequisitionReportDtoBuilder requisitionReportDtoBuilder;

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

  @Test
  public void shouldPrintRequisition()
      throws JasperReportViewException, IOException, JRException {
    // given
    HttpServletRequest request = mock(HttpServletRequest.class);
    ModelAndView view = new ModelAndView();

    when(requisitionRepository.findOne(any(UUID.class))).thenReturn(mock(Requisition.class));
    when(jasperReportsViewService.getRequisitionJasperReportView(
        any(Requisition.class), any(HttpServletRequest.class))).thenReturn(view);

    // when
    ModelAndView result = reportsController.print(request, UUID.randomUUID());

    // then
    assertEquals(result, view);
  }
}
