package org.openlmis.requisition.dto;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.UUID;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
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
