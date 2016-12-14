package org.openlmis.requisition.service;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.service.referencedata.OrderableProductReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

import static java.util.stream.Collectors.toList;

@Service
public class RequisitionLineCalculationService {

  @Autowired
  private OrderableProductReferenceDataService orderableProductReferenceDataService;

  /**
   * Calculates Packs to Ship for all RequisitionLineItems in this Requisition.
   */
  public void calculatePacksToShip(Requisition requisition) {
    requisition.forEachLine(this::calculatePacksToShip);
  }

  /**
   * Calculates Packs to Ship (V) value.
   */
  private void calculatePacksToShip(RequisitionLineItem lineItem) {
    OrderableProductDto orderableProductDto =
        orderableProductReferenceDataService.findOne(lineItem.getOrderableProductId());

    Integer orderQuantity = lineItem.getOrderQuantity();

    if (orderQuantity != null) {
      long packsToShip = orderableProductDto.packsToOrder(orderQuantity.longValue());
      lineItem.setPacksToShip(packsToShip);
    }
  }

  /**
   * Return list of RequisitionLineItemDtos for a given RequisitionLineItem.
   *
   * @param requisitionLineItems List of RequisitionLineItems to be exported to Dto
   * @return list of RequisitionLineItemDtos
   */
  public List<RequisitionLineItemDto> exportToDtos(
      List<RequisitionLineItem> requisitionLineItems) {
    return requisitionLineItems.stream().map(this::exportToDto).collect(toList());
  }

  private RequisitionLineItemDto exportToDto(RequisitionLineItem requisitionLineItem) {
    RequisitionLineItemDto dto = new RequisitionLineItemDto();
    OrderableProductDto orderableProductDto = orderableProductReferenceDataService.findOne(
        requisitionLineItem.getOrderableProductId());
    requisitionLineItem.export(dto, orderableProductDto);
    return dto;
  }

}
