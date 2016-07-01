package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.RequisitionLine;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface RequisitionLineRepository
    extends PagingAndSortingRepository<RequisitionLine, UUID> {
}
