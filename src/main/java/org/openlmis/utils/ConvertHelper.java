/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

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
