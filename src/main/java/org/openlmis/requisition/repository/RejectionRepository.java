package org.openlmis.requisition.repository;

import org.openlmis.requisition.domain.Rejection;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface RejectionRepository extends PagingAndSortingRepository<Rejection, UUID> {

}
