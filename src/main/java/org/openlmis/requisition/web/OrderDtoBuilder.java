package org.openlmis.requisition.web;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.StatusMessage;
import org.openlmis.requisition.dto.OrderDto;
import org.openlmis.requisition.dto.OrderLineItemDto;
import org.openlmis.requisition.dto.StatusMessageDto;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.repository.StatusMessageRepository;
import org.openlmis.requisition.service.referencedata.BaseReferenceDataService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

@Component
public class OrderDtoBuilder {

  @Autowired
  @Qualifier("facilityReferenceDataService")
  private FacilityReferenceDataService facilities;

  @Autowired
  private PeriodReferenceDataService periods;

  @Autowired
  private StatusMessageRepository statusMessageRepository;

  @Autowired
  @Qualifier("programReferenceDataService")
  private ProgramReferenceDataService programs;

  /**
   * Create a new instance of OrderDto based on data from {@link Requisition}.
   *
   * @param requisition instance used to create {@link OrderDto} (can be {@code null})
   * @return new instance of {@link OrderDto}. {@code null} if passed argument is {@code
   * null}.
   */
  public OrderDto build(Requisition requisition, UserDto user) {
    if (null == requisition) {
      return null;
    }

    OrderDto order = new OrderDto();
    order.setExternalId(requisition.getId());
    order.setEmergency(requisition.getEmergency());
    order.setFacility(getIfPresent(facilities, requisition.getFacilityId()));
    order.setProcessingPeriod(getIfPresent(periods, requisition.getProcessingPeriodId()));
    order.setQuotedCost(BigDecimal.ZERO);

    order.setReceivingFacility(getIfPresent(facilities, requisition.getFacilityId()));
    order.setRequestingFacility(getIfPresent(facilities, requisition.getFacilityId()));

    order.setSupplyingFacility(getIfPresent(facilities, requisition.getSupplyingFacilityId()));
    order.setProgram(getIfPresent(programs, requisition.getProgramId()));
    order.setStatusMessages(getStatusMessages(requisition));

    order.setOrderLineItems(
        requisition
            .getRequisitionLineItems()
            .stream()
            .map(OrderLineItemDto::newOrderLineItem)
            .collect(Collectors.toList())
    );

    order.setCreatedBy(user);

    return order;
  }

  private List<StatusMessageDto> getStatusMessages(Requisition requisition) {
    List<StatusMessageDto> statusMessageDtoList = new ArrayList<>();
    List<StatusMessage> statusMessages = statusMessageRepository.findByRequisitionId(
        requisition.getId());
    for (StatusMessage statusMessage: statusMessages) {
      StatusMessageDto statusMessageDto = new StatusMessageDto();
      statusMessage.export(statusMessageDto);
      statusMessageDto.setId(null);
      statusMessageDtoList.add(statusMessageDto);
    }
    return statusMessageDtoList;
  }

  private <T> T getIfPresent(BaseReferenceDataService<T> service, UUID id) {
    return Optional.ofNullable(id).isPresent() ? service.findOne(id) : null;
  }

}
