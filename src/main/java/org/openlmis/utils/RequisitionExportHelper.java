package org.openlmis.utils;

import static java.util.stream.Collectors.toList;

import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.dto.OrderableDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.service.referencedata.OrderableReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class RequisitionExportHelper {

  @Autowired
  private OrderableReferenceDataService orderableReferenceDataService;

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
    OrderableDto orderableDto = orderableReferenceDataService.findOne(
        requisitionLineItem.getOrderableId());
    requisitionLineItem.export(dto, orderableDto);
    return dto;
  }
}
