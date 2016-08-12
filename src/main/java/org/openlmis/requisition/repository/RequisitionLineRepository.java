package org.openlmis.requisition.repository;

import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.repository.custom.RequisitionLineRepositoryCustom;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface RequisitionLineRepository extends
        PagingAndSortingRepository<RequisitionLine, UUID>,
        RequisitionLineRepositoryCustom {
}
