package org.openlmis.requisition.service;

import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class RequisitionTemplateService {

  @Autowired
  private RequisitionTemplateRepository requisitionTemplateRepository;

  /**
   * Method returns all requisition templates with matched parameters.
   * @param program program of searched requisition templates.
   * @return list of requisition templates with matched parameters.
   */
  public List<RequisitionTemplate> searchRequisitionTemplates(ProgramDto program) {
    return requisitionTemplateRepository.searchRequisitionTemplates(program);
  }
}


