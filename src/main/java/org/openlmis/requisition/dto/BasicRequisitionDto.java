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

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.StatusChange;
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

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
public class BasicRequisitionDto implements Requisition.Exporter {

  private UUID id;
  private Boolean emergency;
  private RequisitionStatus status;
  private Map<String, StatusLogEntry> statusChanges = new HashMap<>();

  @JsonSerialize(as = BasicProcessingPeriodDto.class)
  private BasicProcessingPeriodDto processingPeriod;

  @JsonSerialize(as = BasicFacilityDto.class)
  private BasicFacilityDto facility;

  @JsonSerialize(as = BasicProgramDto.class)
  private BasicProgramDto program;

  @Override
  public void setStatusChanges(List<StatusChange> statusChanges) {
    if (statusChanges == null) {
      return;
    }

    for (StatusChange statusChange : statusChanges) {
      StatusLogEntry existing = this.statusChanges.get(statusChange.getStatus().toString());
      // Only add entry if none exists or existing one has later date
      if (existing == null || existing.getChangeDate().isAfter(statusChange.getCreatedDate())) {
        StatusLogEntry entry = new StatusLogEntry(statusChange.getAuthorId(),
            statusChange.getCreatedDate());
        this.statusChanges.put(statusChange.getStatus().toString(), entry);
      }
    }
  }

  @Override
  public void setCreatedDate(ZonedDateTime createdDate) {
  }

  @Override
  public void setModifiedDate(ZonedDateTime createdDate) {
  }

  @Override
  public void setSupplyingFacility(UUID supplyingFacility) {
  }

  @Override
  public void setSupervisoryNode(UUID supervisoryNode) {
  }

  @Override
  public void setTemplate(RequisitionTemplate template) {
  }

  @Override
  public void setDraftStatusMessage(String draftStatusMessage) {
  }
}
