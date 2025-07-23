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

package org.openlmis.requisition.service.stockmanagement;

import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

public class StockCardRangeSummaryPostRequest {
  private UUID programId;
  private UUID facilityId;
  private List<UUID> orderableIds;
  private String tag;
  private LocalDate startDate;
  private LocalDate endDate;
 
  public UUID getProgramId() {
    return programId;
  }

  public void setProgramId(UUID programId) {
    this.programId = programId;
  }

  public UUID getFacilityId() {
    return facilityId;
  }

  public void setFacilityId(UUID facilityId) {
    this.facilityId = facilityId;
  }

  public List<UUID> getOrderableIds() {
    return orderableIds;
  }

  public void setOrderableIds(List<UUID> orderableIds) {
    this.orderableIds = orderableIds;
  }

  public String getTag() {
    return tag;
  }

  public void setTag(String tag) {
    this.tag = tag;
  }

  public LocalDate getStartDate() {
    return startDate;
  }

  public void setStartDate(LocalDate startDate) {
    this.startDate = startDate;
  }

  public LocalDate getEndDate() {
    return endDate;
  }

  public void setEndDate(LocalDate endDate) {
    this.endDate = endDate;
  }

}
