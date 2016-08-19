package org.openlmis.fulfillment.repository;

import org.openlmis.fulfillment.domain.ProofOfDeliveryLine;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface ProofOfDeliveryLineRepository extends
      PagingAndSortingRepository<ProofOfDeliveryLine, UUID> {
}