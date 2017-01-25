package org.openlmis.requisition.web;

import org.openlmis.requisition.domain.JasperTemplate;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.dto.RequisitionReportDto;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.JasperReportViewException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.JasperReportsViewService;
import org.openlmis.requisition.service.JasperTemplateService;
import org.openlmis.requisition.service.PermissionService;
import org.openlmis.requisition.service.RequisitionService;
import org.openlmis.utils.Message;
import org.openlmis.utils.ReportUtils;
import org.openlmis.utils.RequisitionExportHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.view.jasperreports.JasperReportsMultiFormatView;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;

@Controller
public class ReportsController extends BaseController {
  private static final String REQUISITION_PRINT_TEMPLATE_NAME = "Print Requisition";

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private PermissionService permissionService;

  @Autowired
  private RequisitionDtoBuilder requisitionDtoBuilder;

  @Autowired
  private RequisitionExportHelper requisitionExportHelper;

  @Autowired
  private RequisitionService requisitionService;

  @Autowired
  private JasperTemplateService jasperTemplateService;

  @Autowired
  private JasperReportsViewService jasperReportsViewService;

  /**
   * Print out requisition as a PDF file.
   *
   * @param id The UUID of the requisition to print
   * @return ResponseEntity with the "#200 OK" HTTP response status and PDF file on success, or
   *     ResponseEntity containing the error description status.
   */
  @RequestMapping(value = "/requisitions/{id}/print", method = RequestMethod.GET)
  @ResponseBody
  public ModelAndView print(HttpServletRequest request, @PathVariable("id") UUID id)
      throws JasperReportViewException {
    permissionService.canViewRequisition(id);

    Requisition requisition = requisitionRepository.findOne(id);
    if (requisition == null) {
      throw new ContentNotFoundMessageException(
          new Message(MessageKeys.ERROR_REQUISITION_NOT_FOUND, id));
    }

    JasperTemplate jasperTemplate =
        jasperTemplateService.getByName(REQUISITION_PRINT_TEMPLATE_NAME);
    if (jasperTemplate == null) {
      throw new ValidationMessageException(
          new Message(MessageKeys.ERROR_REPORTING_TEMPLATE_NOT_FOUND));
    }

    List<RequisitionLineItem> fullSupply = requisitionService.getFullSupplyItems(id)
        .stream().filter(l -> !l.getSkipped()).collect(Collectors.toList());
    List<RequisitionLineItem> nonFullSupply = requisitionService.getNonFullSupplyItems(id)
        .stream().filter(l -> !l.getSkipped()).collect(Collectors.toList());

    RequisitionReportDto reportDto = new RequisitionReportDto(
        requisitionDtoBuilder.build(requisition),
        requisitionExportHelper.exportToDtos(fullSupply),
        requisitionExportHelper.exportToDtos(nonFullSupply)
    );

    Map<String, Object> params = ReportUtils.createParametersMap();
    params.put("datasource", Collections.singletonList(reportDto));
    params.put("template", requisition.getTemplate());

    JasperReportsMultiFormatView jasperView =
        jasperReportsViewService.getJasperReportsView(jasperTemplate, request);
    return new ModelAndView(jasperView, params);
  }
}
