package org.openlmis.fulfillment.repository;

import org.openlmis.fulfillment.domain.OrderLineItem;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface OrderLineItemRepository extends PagingAndSortingRepository<OrderLineItem, UUID> {
}
