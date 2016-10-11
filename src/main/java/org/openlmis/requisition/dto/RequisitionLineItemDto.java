package org.openlmis.requisition.dto;

import org.openlmis.requisition.domain.RequisitionLineItem;

import java.util.UUID;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RequisitionLineItemDto implements RequisitionLineItem.Exporter, RequisitionLineItem.Importer{
  private UUID id;
  private OrderableProductDto orderableProduct;
  private Integer stockInHand;
  private Integer beginningBalance;
  private Integer totalReceivedQuantity;
  private Integer totalLossesAndAdjustments;
  private Integer stockOnHand;
  private Integer requestedQuantity;
  private Integer totalConsumedQuantity;
  private String requestedQuantityExplanation;
  private String remarks;
  private Integer approvedQuantity;
}
