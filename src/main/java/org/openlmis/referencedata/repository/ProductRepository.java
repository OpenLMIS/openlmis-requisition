package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Product;
import org.springframework.data.repository.CrudRepository;

import java.util.UUID;

public interface ProductRepository extends CrudRepository<Product, UUID> {
}
