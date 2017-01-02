package org.openlmis.utils;

import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.dto.OrderableProductDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.service.referencedata.OrderableProductReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

import static java.util.stream.Collectors.toList;

@Component
public class RequisitionExportHelper {

  @Autowired
  private OrderableProductReferenceDataService orderableProductReferenceDataService;

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
