package org.openlmis.requisition.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.UUID;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OrderDto {
  private UUID id;
  private UUID externalId;
  private Boolean emergency;
  private FacilityDto facility;
  private ProcessingPeriodDto processingPeriod;
  private ZonedDateTime createdDate;
  private UserDto createdBy;
  private ProgramDto program;
  private FacilityDto requestingFacility;
  private FacilityDto receivingFacility;
  private FacilityDto supplyingFacility;
  private String orderCode;
  private OrderStatus status;
  private BigDecimal quotedCost;
  private List<OrderLineItemDto> orderLineItems;
  private List<StatusMessageDto> statusMessages;
}
