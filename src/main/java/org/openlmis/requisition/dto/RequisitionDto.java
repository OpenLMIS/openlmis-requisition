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

import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.StatusChange;
import org.openlmis.requisition.domain.StatusLogEntry;
import org.openlmis.utils.StatusChangeHelper;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

@AllArgsConstructor
@NoArgsConstructor
public class RequisitionDto implements Requisition.Importer, Requisition.Exporter {
  @Getter
  @Setter
  private UUID id;

  @Getter
  @Setter
  private ZonedDateTime createdDate;

  @Getter
  @Setter
  private ZonedDateTime modifiedDate;

  @Setter
  private List<RequisitionLineItemDto> requisitionLineItems;

  @Getter
  @Setter
  private String draftStatusMessage;

  @Getter
  @Setter
  private FacilityDto facility;

  @Getter
  @Setter
  private ProgramDto program;

  @Getter
  @Setter
  private ProcessingPeriodDto processingPeriod;

  @Getter
  @Setter
  private RequisitionStatus status;

  @Getter
  @Setter
  private Boolean emergency;

  @Getter
  @Setter
  private UUID supplyingFacility;

  @Getter
  @Setter
  private UUID supervisoryNode;

  @Getter
  @Setter
  private RequisitionTemplate template;

  @Getter
  @Setter
  private Set<OrderableDto> availableNonFullSupplyProducts;

  @Getter
  private Map<String, StatusLogEntry> statusChanges = new HashMap<>();
  
  @Getter
  private List<StatusChangeDto> statusHistory = new ArrayList<>();

  @Override
  public List<RequisitionLineItem.Importer> getRequisitionLineItems() {
    return new ArrayList<>(
        Optional.ofNullable(requisitionLineItems).orElse(Collections.emptyList())
    );
  }

  @Override
  public void setStatusChanges(List<StatusChange> statusChanges) {
    if (statusChanges == null) {
      return;
    }
    
    for (StatusChange statusChange : statusChanges) {
      StatusChangeHelper.addOrUpdate(this.statusChanges, statusChange);
      addToStatusHistory(statusChange);
    }
  }
  
  private void addToStatusHistory(StatusChange statusChange) {
    StatusChangeDto statusChangeDto = new StatusChangeDto();
    statusChange.export(statusChangeDto);
    this.statusHistory.add(statusChangeDto);
  }
}
