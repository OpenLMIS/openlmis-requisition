package org.openlmis.requisition.service;

import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.domain.RequisitionTemplate;
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
   */
  public List<RequisitionTemplate> searchRequisitionTemplates(Program program) {
    return requisitionTemplateRepository.searchRequisitionTemplates(program);
  }
}


