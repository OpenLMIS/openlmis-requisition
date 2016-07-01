package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Requisition;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface RequisitionRepository
    extends PagingAndSortingRepository<Requisition, UUID> {

}
