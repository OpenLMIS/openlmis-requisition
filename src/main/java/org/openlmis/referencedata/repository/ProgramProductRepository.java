package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.ProgramProduct;
import org.openlmis.referencedata.repository.custom.ProgramProductRepositoryCustom;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface ProgramProductRepository extends
        PagingAndSortingRepository<ProgramProduct, UUID>,
        ProgramProductRepositoryCustom {
}
