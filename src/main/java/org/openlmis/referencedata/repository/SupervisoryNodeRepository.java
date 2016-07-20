package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.SupervisoryNode;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface SupervisoryNodeRepository 
    extends PagingAndSortingRepository<SupervisoryNode, UUID> {
}
