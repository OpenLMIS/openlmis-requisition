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
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.StatusChange;
import org.openlmis.requisition.domain.StatusLogEntry;
import org.openlmis.requisition.utils.StatusChangeHelper;
import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
public class BasicRequisitionDto extends BaseDto implements Requisition.Exporter {

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

  @Override
  public void addStatusChange(StatusChange.Exporter statusChangeExporter) {
    StatusChangeDto statusChangeDto = (StatusChangeDto) statusChangeExporter;
    StatusChangeHelper.addOrUpdate(this.statusChanges, statusChangeDto);
  }

  @Override
  public void setCreatedDate(ZonedDateTime createdDate) {
    // unsupported operation
  }

  @Override
  public void setSupplyingFacility(UUID supplyingFacility) {
    // unsupported operation
  }

  @Override
  public void setSupervisoryNode(UUID supervisoryNode) {
    // unsupported operation
  }

  @Override
  public void setTemplate(BasicRequisitionTemplateDto template) {
    // unsupported operation
  }

  @Override
  public void setDraftStatusMessage(String draftStatusMessage) {
    // unsupported operation
  }

  @Override
  public void setDatePhysicalStockCountCompleted(LocalDate localDate) {
    // unsupported operation
  }

  @Override
  public void setStockAdjustmentReasons(List<ReasonDto> reasonDto) {
    // unsupported operation
  }

  @Override
  public Optional<StatusChange.Exporter> provideStatusChangeExporter() {
    return Optional.of(new StatusChangeDto());
  }
}
