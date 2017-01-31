package org.openlmis.requisition.service;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.i18n.MessageKeys;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.openlmis.utils.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Service
public class RequisitionTemplateService {

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  /**
   * Method returns Requisition templates with matched parameters.
   *
   * @param program Program of searched requisition template.
   * @return RequisitionTemplate with matched parameters.
   */
  public RequisitionTemplate getTemplateForProgram(UUID program) {
    return requisitionTemplateRepository.getTemplateForProgram(program);
  }

  /**
   * Method saves given requisition template. When an update occurs,
   * and the template is already referred by a requisition, a new template is created.
   *
   * @param template Template to be saved.
   * @return Saved template.
   */
  public RequisitionTemplate save(RequisitionTemplate template) {
    List<Requisition> requisitions = requisitionRepository.findByTemplateId(template.getId());
    if (!requisitions.isEmpty()) {
      Map<String, RequisitionTemplateColumn> columnsMap = new HashMap<>();
      template.getColumnsMap().forEach(columnsMap::put);

      RequisitionTemplate newTemplate = new RequisitionTemplate(columnsMap);
      newTemplate.updateFrom(template);
      template = newTemplate;
    }

    return requisitionTemplateRepository.save(template);
  }

  /**
   * Safe delete of a requisition template. If the requisition template is used by any
   * requisition, a {@link ValidationMessageException} signals that it cannot be removed.
   *
   * @param template the template to remove
   */
  public void delete(RequisitionTemplate template) {
    if (!requisitionRepository.findByTemplateId(template.getId()).isEmpty()) {
      throw new ValidationMessageException(new Message(
          MessageKeys.ERROR_REQUISITION_TEMPLATE_IN_USE,
          template.getId()));
    } else {
      requisitionTemplateRepository.delete(template);
    }
  }
}
