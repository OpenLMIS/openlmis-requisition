package org.openlmis.requisition.dto;

import org.openlmis.fulfillment.domain.OrderStatus;
import org.openlmis.requisition.domain.Requisition;

import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Data
public class OrderDto {
  private UUID id;
  private UUID externalId;
  private boolean emergency;
  private UUID facilityId;
  private UUID processingPeriodId;
  private LocalDateTime createdDate;
  private java.util.UUID createdById;
  private UUID programId;
  private UUID requestingFacilityId;
  private UUID receivingFacilityId;
  private UUID supplyingFacilityId;
  private String orderCode;
  private OrderStatus status;
  private BigDecimal quotedCost;
  private List<OrderLineItemDto> orderLineItems;

  /**
   * Static factory method for constructing new Order based on Requisition.
   *
   * @param requisition Requisition to create instance from.
   */
  public static OrderDto newOrder(Requisition requisition) {
    OrderDto order = new OrderDto();
    order.setExternalId(requisition.getId());
    order.setEmergency(requisition.getEmergency());
    order.setFacilityId(requisition.getFacilityId());
    order.setProcessingPeriodId(requisition.getProcessingPeriodId());
    order.setStatus(OrderStatus.ORDERED);
    order.setQuotedCost(BigDecimal.ZERO);

    order.setReceivingFacilityId(requisition.getFacilityId());
    order.setRequestingFacilityId(requisition.getFacilityId());

    order.setSupplyingFacilityId(requisition.getSupplyingFacilityId());
    order.setProgramId(requisition.getProgramId());

    order.setOrderLineItems(
        requisition
            .getRequisitionLineItems()
            .stream()
            .map(OrderLineItemDto::newOrderLineItem)
            .collect(Collectors.toList())
    );

    return order;
  }

  /**
   * Static factory method for constructing new Order based on Requisition and User.
   *
   * @param requisition Requisition to create instance from.
   * @param user        User details
   */
  public static OrderDto newOrder(Requisition requisition, UserDto user) {
    OrderDto order = OrderDto.newOrder(requisition);
    order.setCreatedById(user.getId());

    return order;
  }
}
