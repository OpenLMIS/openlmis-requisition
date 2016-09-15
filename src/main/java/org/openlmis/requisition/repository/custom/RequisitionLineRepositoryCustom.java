package org.openlmis.requisition.repository.custom;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;
import org.openlmis.requisition.dto.ProductDto;

import java.util.List;

public interface RequisitionLineRepositoryCustom {

  List<RequisitionLine> searchRequisitionLines(Requisition requisition, ProductDto product);
}
