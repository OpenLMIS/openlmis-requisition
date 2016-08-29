package org.openlmis.fulfillment.repository;

import org.openlmis.fulfillment.domain.OrderLine;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface OrderLineRepository extends PagingAndSortingRepository<OrderLine, UUID> {
}
