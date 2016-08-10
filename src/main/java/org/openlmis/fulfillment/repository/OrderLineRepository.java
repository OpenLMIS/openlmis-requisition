package org.openlmis.fulfillment.repository;

import org.openlmis.fulfillment.domain.OrderLine;
import org.springframework.data.repository.Repository;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.UUID;

public interface OrderLineRepository extends Repository<OrderLine, UUID> {

  @RestResource(exported = false)
  void deleteAll();

  OrderLine save(OrderLine entity);

  Iterable<OrderLine> save(Iterable<OrderLine> entities);

  /**
   * Retrieves an entity by its id.
   *
   * @param id must not be {@literal null}.
   * @return the entity with the given id or {@literal null} if none found
   * @throws IllegalArgumentException if {@code id} is {@literal null}
   */
  @RestResource(exported = false)
  OrderLine findOne(UUID id);
}
