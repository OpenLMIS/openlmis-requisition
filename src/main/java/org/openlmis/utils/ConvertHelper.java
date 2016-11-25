package org.openlmis.utils;

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.web.RequisitionDtoBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class ConvertHelper {

  @Autowired
  private RequisitionDtoBuilder requisitionDtoBuilder;

  /**
   * Get RequisitionDto list from Requisition List.
   */
  public List<RequisitionDto> convertRequisitionListToRequisitionDtoList(
      List<Requisition> requisitions) {
    List<RequisitionDto> requisitionsConvertedToDto = new ArrayList<>();

    RequisitionDto requisitionDto;
    for (Requisition requisition : requisitions) {
      requisitionDto = requisitionDtoBuilder.build(requisition);
      requisitionsConvertedToDto.add(requisitionDto);
    }

    return requisitionsConvertedToDto;
  }
}
