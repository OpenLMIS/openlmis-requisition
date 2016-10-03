package org.openlmis.requisition.repository;

import org.openlmis.requisition.domain.RequisitionColumn;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface RequisitionColumnRepository
    extends PagingAndSortingRepository<RequisitionColumn, UUID> {
}
