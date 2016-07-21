package org.openlmis.hierarchyandsupervision.repository;

import org.openlmis.hierarchyandsupervision.domain.SupervisoryNode;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface SupervisoryNodeRepository 
    extends PagingAndSortingRepository<SupervisoryNode, UUID> {
}
