package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.FacilityTypeApprovedProduct;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface FacilityTypeApprovedProductRepository extends PagingAndSortingRepository<FacilityTypeApprovedProduct, UUID> {
}
