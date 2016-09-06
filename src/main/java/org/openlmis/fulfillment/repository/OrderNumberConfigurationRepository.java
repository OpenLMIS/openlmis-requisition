package org.openlmis.fulfillment.repository;

import org.openlmis.fulfillment.domain.OrderNumberConfiguration;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;


public interface OrderNumberConfigurationRepository
    extends PagingAndSortingRepository<OrderNumberConfiguration, UUID>  {
}
