package org.openlmis.requisition.dto;

import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

@Getter
@Setter
public class RequisitionLineItemDto {
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
