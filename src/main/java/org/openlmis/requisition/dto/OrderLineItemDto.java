package org.openlmis.requisition.dto;

import org.openlmis.requisition.domain.RequisitionLineItem;

import lombok.Data;

@Data
public class OrderLineItemDto {
  private OrderableDto orderable;
  private Long orderedQuantity;
  private Long filledQuantity;
  private Long approvedQuantity;
  private Long packsToShip;

  /**
   * Static factory method for constructing new OrderLineItem based on RequisitionLineItem.
   *
   * @param lineItem RequisitionLineItem to create instance from.
   */
  public static OrderLineItemDto newOrderLineItem(RequisitionLineItem lineItem,
                                                  OrderableDto productDto) {
    OrderLineItemDto orderLineItem = new OrderLineItemDto();
    orderLineItem.setOrderable(productDto);
    orderLineItem.setFilledQuantity(0L);
    orderLineItem.setOrderedQuantity(lineItem.getRequestedQuantity().longValue());
    orderLineItem.setApprovedQuantity(lineItem.getApprovedQuantity().longValue());
    orderLineItem.setPacksToShip(lineItem.getPacksToShip());

    return orderLineItem;
  }
}
