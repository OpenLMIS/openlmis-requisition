package org.openlmis.requisition.repository;

import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.repository.custom.RequisitionLineItemRepositoryCustom;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface RequisitionLineItemRepository extends
        PagingAndSortingRepository<RequisitionLineItem, UUID>,
    RequisitionLineItemRepositoryCustom {
}
