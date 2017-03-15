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

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_COLUMN_iS_NOT_VALID_FOR_SORTING;

import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.exception.ValidationMessageException;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

import java.util.Comparator;

@NoArgsConstructor
@AllArgsConstructor
public class RequisitionDtoComparator implements Comparator<RequisitionDto> {

  private String compareCondition;

  @Override
  public int compare(RequisitionDto o1, RequisitionDto o2) {
    switch (compareCondition) {
      case "programName": {
        return o1.getProgram().getName().compareTo(o2.getProgram().getName());
      }
      case "facilityCode": {
        return o1.getFacility().getCode().compareTo(o2.getFacility().getCode());
      }
      case "facilityName": {
        return o1.getFacility().getName().compareTo(o2.getFacility().getName());
      }
      default: {
        throw new ValidationMessageException(new Message(ERROR_COLUMN_iS_NOT_VALID_FOR_SORTING,
            compareCondition));
      }
    }
  }
}
