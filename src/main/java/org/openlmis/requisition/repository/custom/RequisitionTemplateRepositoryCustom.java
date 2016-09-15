package org.openlmis.requisition.repository.custom;

import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.ProgramDto;

import java.util.List;

public interface RequisitionTemplateRepositoryCustom {

  List<RequisitionTemplate> searchRequisitionTemplates(ProgramDto program);
}
