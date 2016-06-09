package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.OrderLine;
import org.springframework.data.repository.Repository;

public interface OrderLineRepository extends Repository<OrderLine, Integer> {

  OrderLine save(OrderLine entity);

  Iterable<OrderLine> save(Iterable<OrderLine> entities);
}
