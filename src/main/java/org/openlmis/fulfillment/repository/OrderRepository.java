package org.openlmis.fulfillment.repository;

import org.openlmis.fulfillment.domain.Order;
import org.openlmis.fulfillment.repository.custom.OrderRepositoryCustom;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface OrderRepository extends
        PagingAndSortingRepository<Order, UUID>,
        OrderRepositoryCustom {

  @Override
  @RestResource(exported = false)
  void delete(Order entity);

  @Override
  @RestResource(exported = false)
  void delete(UUID id);

  @Override
  @RestResource(exported = false)
  void delete(Iterable<? extends Order> entities);

  @Override
  @RestResource(exported = false)
  void deleteAll();
}
