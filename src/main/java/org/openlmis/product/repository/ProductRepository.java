package org.openlmis.product.repository;

import org.openlmis.product.domain.Product;
import org.openlmis.referencedata.repository.ReferenceDataRepository;

import java.util.UUID;

public interface ProductRepository extends ReferenceDataRepository<Product, UUID> {
    //Add custom Product related members here. See UserRepository.java for examples.
}
