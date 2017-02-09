package org.openlmis.requisition.dto;


import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@AllArgsConstructor
@NoArgsConstructor
@Data
public class ProofOfDeliveryLineItemDto {
  private UUID id;
  private OrderLineItemDto orderLineItem;
  private Long packsToShip;
  private Long quantityShipped;
  private Long quantityReceived;
  private Long quantityReturned;
  private String replacedProductCode;
  private String notes;
}
