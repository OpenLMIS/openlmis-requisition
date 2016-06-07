package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Facility;
import org.openlmis.referencedata.domain.FacilityType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.repository.Repository;

public interface FacilityTypeRepository extends Repository<FacilityType, Integer>
{
    Iterable<FacilityType> findAll(Sort sort);
    Page<FacilityType> findAll(Pageable pageable);

    //See UserRepository for examples of how we might implement additional endpoints after we know what they should be.
}
