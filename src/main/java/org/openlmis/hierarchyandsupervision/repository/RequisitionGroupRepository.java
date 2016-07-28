package org.openlmis.hierarchyandsupervision.repository;

import org.openlmis.hierarchyandsupervision.domain.RequisitionGroup;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface RequisitionGroupRepository
    extends PagingAndSortingRepository<RequisitionGroup, UUID> {
}
