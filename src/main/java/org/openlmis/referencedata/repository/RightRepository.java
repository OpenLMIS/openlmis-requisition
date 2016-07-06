package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Right;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface RightRepository extends ReferenceDataRepository<Right, UUID> {
  //Accessible via http://127.0.0.1:8080/api/rights/search/findByName?name={name}
  Right findByName(@Param("name") String name);
}
