package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.RequisitionGroup;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface RequisitionGroupRepository
    extends PagingAndSortingRepository<RequisitionGroup, UUID> {
}
