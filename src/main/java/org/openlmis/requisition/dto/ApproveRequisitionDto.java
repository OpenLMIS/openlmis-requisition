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

package org.openlmis.requisition.dto;

import org.openlmis.requisition.domain.RequisitionStatus;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.ZonedDateTime;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class ApproveRequisitionDto {
  private UUID id;
  private Boolean emergency;
  private RequisitionStatus status;
  private ZonedDateTime modifiedDate;
  private String periodName;
  private String facilityName;

  private List<ApproveRequisitionLineItemDto> requisitionLineItems;

  /**
   * Creates instance with data from original requisition.
   */
  public ApproveRequisitionDto(RequisitionDto requisition) {
    this.id = requisition.getId();
    this.emergency = requisition.getEmergency();
    this.status = requisition.getStatus();
    this.modifiedDate = requisition.getModifiedDate();
    this.periodName = requisition.getProcessingPeriod().getName();
    this.facilityName = requisition.getFacility().getName();
    this.requisitionLineItems = requisition
        .getRequisitionLineItems()
        .stream()
        .map(ApproveRequisitionLineItemDto::new)
        .collect(Collectors.toList());
  }

}
