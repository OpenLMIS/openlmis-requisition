package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.OrderLine;
import org.springframework.data.repository.Repository;

import java.util.UUID;

public interface OrderLineRepository extends Repository<OrderLine, UUID> {

  OrderLine save(OrderLine entity);

  Iterable<OrderLine> save(Iterable<OrderLine> entities);
}
