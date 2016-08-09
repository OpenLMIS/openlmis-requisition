package org.openlmis.requisition.repository.custom;

import org.openlmis.product.domain.Product;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLine;

import java.util.List;

public interface RequisitionLineRepositoryCustom {

  List<RequisitionLine> searchRequisitionLines(Requisition requisition, Product product);
}
