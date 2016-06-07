package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.GeographicLevel;
import org.openlmis.referencedata.domain.GeographicZone;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.repository.Repository;

public interface GeographicZoneRepository extends Repository<GeographicZone, Integer>
{
    Iterable<GeographicZone> findAll(Sort sort);
    Page<GeographicZone> findAll(Pageable pageable);

    //See UserRepository for examples of how we might implement additional endpoints after we know what they should be.
}
