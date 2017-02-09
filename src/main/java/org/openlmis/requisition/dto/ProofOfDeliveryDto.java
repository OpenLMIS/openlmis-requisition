package org.openlmis.requisition.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.ZonedDateTime;
import java.util.List;
import java.util.Objects;
import java.util.UUID;


@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class ProofOfDeliveryDto {
  private UUID id;
  private OrderDto order;
  private List<ProofOfDeliveryLineItemDto> proofOfDeliveryLineItems;
  private String deliveredBy;
  private String receivedBy;
  private ZonedDateTime receivedDate;

  @JsonIgnore
  public boolean isSubmitted() {
    return null != order && OrderStatus.RECEIVED == order.getStatus();
  }

  /**
   * Finds a correct line item based on product id.
   */
  public ProofOfDeliveryLineItemDto findLineByProductId(UUID productId) {
    if (null == proofOfDeliveryLineItems) {
      return null;
    }

    return proofOfDeliveryLineItems
        .stream()
        .filter(e -> null != e.getOrderLineItem() && null != e.getOrderLineItem().getOrderable())
        .filter(e -> Objects.equals(productId, e.getOrderLineItem().getOrderable().getId()))
        .findFirst()
        .orElse(null);
  }
}
