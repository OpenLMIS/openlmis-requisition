package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Facility;
import org.springframework.data.repository.CrudRepository;

import java.util.UUID;

public interface FacilityRepository extends CrudRepository<Facility, UUID> {
}
