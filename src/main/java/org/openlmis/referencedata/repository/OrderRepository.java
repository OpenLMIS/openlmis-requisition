package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Order;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface OrderRepository extends PagingAndSortingRepository<Order, UUID> {
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
