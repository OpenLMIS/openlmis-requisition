package org.openlmis.requisition.repository.custom;

import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.domain.RequisitionTemplate;

import java.util.List;

public interface RequisitionTemplateRepositoryCustom {

  List<RequisitionTemplate> searchRequisitionTemplates(Program program);
}
