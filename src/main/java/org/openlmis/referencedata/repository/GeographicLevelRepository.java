package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.GeographicLevel;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.repository.Repository;

public interface GeographicLevelRepository extends Repository<GeographicLevel, Integer>
{
    Iterable<GeographicLevel> findAll(Sort sort);
    Page<GeographicLevel> findAll(Pageable pageable);

    //See UserRepository for examples of how we might implement additional endpoints after we know what they should be.
}
