package org.openlmis.requisition.repository;

import org.openlmis.requisition.domain.Product;
import org.springframework.data.repository.CrudRepository;

public interface ProductRepository extends CrudRepository<Product, Integer> {
}
