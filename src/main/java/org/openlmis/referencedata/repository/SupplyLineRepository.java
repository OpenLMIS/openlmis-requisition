package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.SupplyLine;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface SupplyLineRepository extends PagingAndSortingRepository<SupplyLine, UUID> {
}
