package org.openlmis.fulfillment.repository;

import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface ProofOfDeliveryRepository extends
    PagingAndSortingRepository<ProofOfDelivery, UUID> {
}

