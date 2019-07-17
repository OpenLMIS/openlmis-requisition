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

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import org.openlmis.requisition.domain.StatusLogEntry;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.StatusChange;
import org.openlmis.requisition.utils.StatusChangeHelper;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public final class RequisitionV2Dto
    extends BaseDto
    implements Requisition.Importer, Requisition.Exporter {
  private ZonedDateTime createdDate;
  private ZonedDateTime modifiedDate;
  private List<RequisitionLineItemV2Dto> requisitionLineItems;
  private String draftStatusMessage;
  private ObjectReferenceDto facility;
  private ObjectReferenceDto program;
  private ObjectReferenceDto processingPeriod;
  private RequisitionStatus status;
  private Boolean emergency;
  private Boolean reportOnly;
  private UUID supplyingFacility;
  private UUID supervisoryNode;
  private BasicRequisitionTemplateDto template;
  private Set<VersionObjectReferenceDto> availableFullSupplyProducts;
  private Set<VersionObjectReferenceDto> availableNonFullSupplyProducts;
  private Map<String, StatusLogEntry> statusChanges = new HashMap<>();
  private List<StatusChangeDto> statusHistory = new ArrayList<>();
  private LocalDate datePhysicalStockCountCompleted;
  private List<ReasonDto> stockAdjustmentReasons;
  private Map<String, Object> extraData;

  @Override
  public List<RequisitionLineItem.Importer> getRequisitionLineItems() {
    return new ArrayList<>(
        Optional.ofNullable(requisitionLineItems).orElse(Collections.emptyList())
    );
  }

  @Override
  @JsonIgnore
  public UUID getFacilityId() {
    return Optional
        .ofNullable(facility)
        .map(BaseDto::getId)
        .orElse(null);
  }

  @Override
  @JsonIgnore
  public UUID getProgramId() {
    return Optional
        .ofNullable(program)
        .map(BaseDto::getId)
        .orElse(null);
  }

  @Override
  @JsonIgnore
  public UUID getProcessingPeriodId() {
    return Optional
        .ofNullable(processingPeriod)
        .map(BaseDto::getId)
        .orElse(null);
  }

  @Override
  @JsonIgnore
  public Set<VersionIdentityDto> getAvailableNonFullSupplyProductsIdentities() {
    return Optional
        .ofNullable(availableNonFullSupplyProducts)
        .orElse(Collections.emptySet())
        .stream()
        .map(item -> new VersionIdentityDto(item.getId(), item.getVersionId()))
        .collect(Collectors.toSet());
  }

  @Override
  public void addStatusChange(StatusChange.Exporter statusChangeExporter) {
    StatusChangeDto statusChangeDto = (StatusChangeDto) statusChangeExporter;
    StatusChangeHelper.addOrUpdate(this.statusChanges, statusChangeDto);
    statusHistory.add(statusChangeDto);
  }

  @Override
  public Optional<Supplier<StatusChange.Exporter>> provideStatusChangeExporter() {
    return Optional.of(StatusChangeDto::new);
  }
}
