package org.openlmis.requisition.repository.custom;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;

import java.util.List;
import java.util.UUID;

public interface RequisitionLineRepositoryCustom {

  List<RequisitionLine> searchRequisitionLines(Requisition requisition, UUID product);
}
