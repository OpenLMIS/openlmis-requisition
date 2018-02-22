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

package org.openlmis.requisition.testutils;

import org.assertj.core.util.Lists;
import org.assertj.core.util.Sets;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.requisition.DatePhysicalStockCountCompleted;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionLineItem;
import org.openlmis.requisition.domain.requisition.RequisitionPermissionString;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.domain.requisition.StatusChange;
import org.openlmis.requisition.domain.requisition.StockAdjustmentReason;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;

public class RequisitionDataBuilder {
  private UUID id = UUID.randomUUID();
  private List<RequisitionLineItem> requisitionLineItems = Lists.newArrayList();
  private String draftStatusMessage = "";
  private RequisitionTemplate template = new RequisitionTemplateDataBuilder().buildWithAllColumns();
  private UUID facilityId = UUID.randomUUID();
  private UUID programId = UUID.randomUUID();
  private UUID processingPeriodId = UUID.randomUUID();
  private UUID supplyingFacilityId = null;
  private RequisitionStatus status = RequisitionStatus.INITIATED;
  private List<StatusChange> statusChanges = new ArrayList<>();
  private Boolean emergency = false;
  private Integer numberOfMonthsInPeriod = 1;
  private UUID supervisoryNodeId = null;
  private List<Requisition> previousRequisitions = Lists.newArrayList();
  private Set<UUID> availableProducts = Sets.newHashSet();
  private DatePhysicalStockCountCompleted datePhysicalStockCountCompleted =
      new DatePhysicalStockCountCompleted(LocalDate.now().minusMonths(1));
  private List<StockAdjustmentReason> stockAdjustmentReasons = new ArrayList<>();
  private List<RequisitionPermissionString> permissionStrings = new ArrayList<>();

  /**
   * Add a requisition line item. Update available products list only if requisition is
   * emergency or line item is related with non full supply product.
   */
  public RequisitionDataBuilder addLineItem(RequisitionLineItem lineItem) {
    requisitionLineItems.add(lineItem);

    if (emergency || lineItem.isNonFullSupply()) {
      availableProducts.add(lineItem.getOrderableId());
    }

    return this;
  }

  /**
   * Creates new instance of {@link RequisitionLineItem} with passed data.
   */
  public Requisition build() {
    return buildInitiatedRegularRequisition();
  }
  
  /**
   * Creates new instance of {@link RequisitionLineItem} with passed data.
   */
  public Requisition buildInitiatedRegularRequisition() {
    Requisition requisition = new Requisition(
        requisitionLineItems, draftStatusMessage, template, facilityId, programId,
        processingPeriodId, supplyingFacilityId, status, statusChanges, emergency,
        numberOfMonthsInPeriod, supervisoryNodeId, previousRequisitions, availableProducts,
        datePhysicalStockCountCompleted, stockAdjustmentReasons, permissionStrings
    );
    requisition.setId(id);
    requisitionLineItems.forEach(line -> line.setRequisition(requisition));

    return requisition;
  }

  /**
   * Sets datePhysicalStockCountCompleted.
   */
  public RequisitionDataBuilder setDatePhysicalStockCountCompleted(
      LocalDate datePhysicalStockCountCompleted) {
    this.datePhysicalStockCountCompleted =
        new DatePhysicalStockCountCompleted(datePhysicalStockCountCompleted);
    return this;
  }

  /**
   * Sets status.
   */
  public RequisitionDataBuilder setStatus(RequisitionStatus status) {
    this.status = status;
    return this;
  }

  /**
   * Sets line item list.
   */
  public RequisitionDataBuilder withLineItems(List<RequisitionLineItem> requisitionLineItems) {
    this.requisitionLineItems.clear();
    requisitionLineItems.forEach(this::addLineItem);

    return this;
  }

  public RequisitionDataBuilder withFacilityId(UUID facilityId) {
    this.facilityId = facilityId;
    return this;
  }

  public RequisitionDataBuilder withProgramId(UUID programId) {
    this.programId = programId;
    return this;
  }

  public RequisitionDataBuilder withProcessingPeriodId(UUID processingPeriodId) {
    this.processingPeriodId = processingPeriodId;
    return this;
  }

  public RequisitionDataBuilder withSupervisoryNodeId(UUID supervisoryNodeId) {
    this.supervisoryNodeId = supervisoryNodeId;
    return this;
  }
}
