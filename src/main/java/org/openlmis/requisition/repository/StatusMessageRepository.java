package org.openlmis.requisition.repository;

import org.openlmis.requisition.domain.StatusMessage;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.List;
import java.util.UUID;

public interface StatusMessageRepository
    extends PagingAndSortingRepository<StatusMessage, UUID> {
  
  List<StatusMessage> findByRequisitionId(UUID requisitionId);
}
