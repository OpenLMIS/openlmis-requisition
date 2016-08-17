package org.openlmis.hierarchyandsupervision.repository;

import org.openlmis.hierarchyandsupervision.domain.SupplyLine;
import org.openlmis.hierarchyandsupervision.repository.custom.SupplyLineRepositoryCustom;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface SupplyLineRepository extends
        PagingAndSortingRepository<SupplyLine, UUID>,
        SupplyLineRepositoryCustom {
}
