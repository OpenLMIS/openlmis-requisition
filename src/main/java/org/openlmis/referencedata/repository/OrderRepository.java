package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Order;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.rest.core.annotation.RestResource;

public interface OrderRepository extends CrudRepository<Order, Integer> {

    // disable DELETE
    @Override
    @RestResource(exported = false)
    void delete(Order o);
}
