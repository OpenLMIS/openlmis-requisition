package org.openlmis.requisition.repository.custom;

import org.openlmis.requisition.domain.RequisitionTemplate;

import java.util.List;
import java.util.UUID;

public interface RequisitionTemplateRepositoryCustom {

  List<RequisitionTemplate> searchRequisitionTemplates(UUID program);
}
