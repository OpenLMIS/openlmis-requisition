package org.openlmis.requisition.repository.custom;

import org.openlmis.requisition.domain.RequisitionTemplate;

import java.util.UUID;

public interface RequisitionTemplateRepositoryCustom {

  RequisitionTemplate getTemplateForProgram(UUID program);
}
