package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Product;
import org.springframework.data.repository.CrudRepository;

public interface ProductRepository extends CrudRepository<Product, Integer> {
}
