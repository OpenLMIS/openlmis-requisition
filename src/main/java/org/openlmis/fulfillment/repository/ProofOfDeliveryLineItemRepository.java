package org.openlmis.fulfillment.repository;

import org.openlmis.fulfillment.domain.ProofOfDeliveryLineItem;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface ProofOfDeliveryLineItemRepository extends
      PagingAndSortingRepository<ProofOfDeliveryLineItem, UUID> {
}