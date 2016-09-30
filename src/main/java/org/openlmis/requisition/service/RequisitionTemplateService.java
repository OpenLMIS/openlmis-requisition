package org.openlmis.requisition.service;

import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class RequisitionTemplateService {

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
}


