package org.openlmis.fulfillment.repository;

import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface ProofOfDeliveryRepository extends
        PagingAndSortingRepository<ProofOfDelivery, UUID> {

    @RestResource(exported = false)
    void deleteAll();
}

