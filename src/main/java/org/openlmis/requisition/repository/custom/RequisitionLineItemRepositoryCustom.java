package org.openlmis.requisition.repository.custom;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;

import java.util.List;
import java.util.UUID;

public interface RequisitionLineItemRepositoryCustom {

  List<RequisitionLineItem> searchRequisitionLineItems(Requisition requisition, UUID product);
}
