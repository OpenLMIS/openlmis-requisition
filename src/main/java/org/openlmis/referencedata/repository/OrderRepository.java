package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Order;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface OrderRepository extends PagingAndSortingRepository<Order, UUID>
{

  // disable DELETE
  @Override
  @RestResource(exported = false)
  void delete(Order o);
}
