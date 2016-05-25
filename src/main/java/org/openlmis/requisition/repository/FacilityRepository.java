package org.openlmis.requisition.repository;

import org.openlmis.requisition.domain.Facility;
import org.springframework.data.repository.CrudRepository;

public interface FacilityRepository extends CrudRepository<Facility, Integer> {
}
