package org.openlmis.requisition.repository;

import org.openlmis.requisition.domain.AvailableRequisitionColumnOption;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface AvailableRequisitionColumnOptionRepository
    extends PagingAndSortingRepository<AvailableRequisitionColumnOption, UUID> {
}
