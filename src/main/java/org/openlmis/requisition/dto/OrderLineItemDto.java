package org.openlmis.requisition.dto;

import org.openlmis.requisition.domain.RequisitionLineItem;

import lombok.Data;

import java.util.UUID;

@Data
public class OrderLineItemDto {
  private UUID id;
  private UUID orderableProductId;
  private Long orderedQuantity;
  private Long filledQuantity;
  private Long approvedQuantity;

  /**
   * Static factory method for constructing new OrderLineItem based on RequisitionLineItem.
   *
   * @param lineItem RequisitionLineItem to create instance from.
   */
  public static OrderLineItemDto newOrderLineItem(RequisitionLineItem lineItem) {
    OrderLineItemDto orderLineItem = new OrderLineItemDto();
    orderLineItem.setOrderableProductId(lineItem.getOrderableProductId());
    orderLineItem.setFilledQuantity(0L);
    orderLineItem.setOrderedQuantity(lineItem.getRequestedQuantity().longValue());
    orderLineItem.setApprovedQuantity(lineItem.getApprovedQuantity().longValue());

    return orderLineItem;
  }
}
