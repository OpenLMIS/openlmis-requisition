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

import com.fasterxml.jackson.databind.annotation.JsonSerialize;

import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusLogEntry;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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

  private Map<String, StatusLogEntry> statusChanges = new HashMap<>();

  @JsonSerialize(as = BasicProcessingPeriodDto.class)
  private BasicProcessingPeriodDto processingPeriod;

  @JsonSerialize(as = MinimalFacilityDto.class)
  private MinimalFacilityDto facility;

  @JsonSerialize(as = BasicProgramDto.class)
  private BasicProgramDto program;
  private List<ApproveRequisitionLineItemDto> requisitionLineItems;

  /**
   * Creates instance with data from original requisition.
   */
  public ApproveRequisitionDto(RequisitionDto requisition) {
    this.id = requisition.getId();
    this.emergency = requisition.getEmergency();
    this.status = requisition.getStatus();
    this.modifiedDate = requisition.getModifiedDate();
    this.statusChanges = requisition.getStatusChanges();
    this.processingPeriod = requisition.getProcessingPeriod();
    this.facility = requisition.getFacility();
    this.program = requisition.getProgram();
    this.requisitionLineItems = requisition
        .getRequisitionLineItems()
        .stream()
        .map(ApproveRequisitionLineItemDto::new)
        .collect(Collectors.toList());
  }

}
