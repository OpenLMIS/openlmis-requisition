package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Product;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.repository.Repository;

import java.util.UUID;

public interface ProductRepository extends Repository<Product, UUID>
{
    Iterable<Product> findAll(Sort sort);
    Page<Product> findAll(Pageable pageable);

    //See UserRepository for examples of how we might implement additional endpoints after we know what they should be.
}
