package org.openlmis.requisition.dto;

import org.openlmis.requisition.domain.Requisition;

import java.util.UUID;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RequisitionLineItemDto {
  private UUID id;
  private OrderableProductDto orderableProduct;
  private Requisition requisition;
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
