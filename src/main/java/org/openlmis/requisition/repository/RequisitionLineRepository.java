package org.openlmis.requisition.repository;

import org.openlmis.product.domain.Product;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface RequisitionLineRepository
    extends PagingAndSortingRepository<RequisitionLine, UUID> {

  RequisitionLine findByRequisitionAndProduct(@Param("requisition")Requisition requisition,
                                              @Param("product")Product product);
}
