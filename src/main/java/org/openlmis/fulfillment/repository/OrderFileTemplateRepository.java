package org.openlmis.fulfillment.repository;

import org.openlmis.fulfillment.domain.OrderFileTemplate;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface OrderFileTemplateRepository extends
    PagingAndSortingRepository<OrderFileTemplate, UUID> {
}
