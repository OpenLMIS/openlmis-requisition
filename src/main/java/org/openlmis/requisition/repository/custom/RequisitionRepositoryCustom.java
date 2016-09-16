package org.openlmis.requisition.repository.custom;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

public interface RequisitionRepositoryCustom {

  List<Requisition> searchRequisitions(UUID facility, UUID program,
                                       LocalDateTime createdDateFrom,
                                       LocalDateTime createdDateTo,
                                       UUID processingPeriod,
                                       UUID supervisoryNode,
                                       RequisitionStatus requisitionStatus);
}
