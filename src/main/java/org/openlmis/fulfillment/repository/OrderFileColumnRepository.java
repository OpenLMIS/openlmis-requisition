package org.openlmis.fulfillment.repository;

import org.openlmis.fulfillment.domain.OrderFileColumn;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface OrderFileColumnRepository extends
    PagingAndSortingRepository<OrderFileColumn, UUID> {
}
