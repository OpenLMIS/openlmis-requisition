package org.openlmis.requisition.service;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
public class RequisitionTemplateService {

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  /**
   * Method returns Requisition templates with matched parameters.
   * @param program Program of searched requisition template.
   * @return RequisitionTemplate with matched parameters.
   */
  public RequisitionTemplate getTemplateForProgram(UUID program) {
    return requisitionTemplateRepository.getTemplateForProgram(program);
  }

  /**
   * Method saves given requisition template. When an update occurs,
   * and the template is already referred by a requisition, a new template is created.
   * @param template Template to be saved.
   * @return Saved template.
   */
  public RequisitionTemplate save(RequisitionTemplate template) {
    List<Requisition> requisitions = requisitionRepository.searchByTemplate(template.getId());
    if (!requisitions.isEmpty()) {
      template.setId(null);
    }

    return requisitionTemplateRepository.save(template);
  }
}
