package org.openlmis.requisition.repository;

import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface AvailableRequisitionColumnRepository
    extends PagingAndSortingRepository<AvailableRequisitionColumn, UUID> {
}
