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
import java.util.Set;
import java.util.UUID;

public class StockCardSummariesV2PostRequest {
  private List<UUID> programIds;
  private UUID facilityId;
  private Set<UUID> orderableIds;
  private String orderableName;
  private String orderableCode;
  private String lotCode;
  private boolean nonEmptyOnly;
  private LocalDate asOfDate;

  // Getters and settersx
  public List<UUID> getProgramIds() {
    return programIds;
  }

  public void setProgramIds(List<UUID> programIds) {
    this.programIds = programIds;
  }

  public UUID getFacilityId() {
    return facilityId; 
  }

  public void setFacilityId(UUID facilityId) {
    this.facilityId = facilityId;
  }

  public Set<UUID> getOrderableIds() {
    return orderableIds;
  }  

  public void setOrderableIds(Set<UUID> orderableIds) {
    this.orderableIds = orderableIds;
  }

  public String getOrderableName() {  
    return orderableName;
  }

  public void setOrderableName(String orderableName) {
    this.orderableName = orderableName;
  }

  public String getOrderableCode() {
    return orderableCode;
  }

  public void setOrderableCode(String orderableCode) {
    this.orderableCode = orderableCode;
  }  

  public String getLotCode() {
    return lotCode;
  }

  public void setLotCode(String lotCode) {
    this.lotCode = lotCode;
  }  

  public boolean isNonEmptyOnly() {
    return nonEmptyOnly;
  }

  public void setNonEmptyOnly(boolean nonEmptyOnly) {
    this.nonEmptyOnly = nonEmptyOnly;
  } 

  public LocalDate getAsOfDate() {
    return asOfDate;
  }

  public void setAsOfDate(LocalDate asOfDate) {
    this.asOfDate = asOfDate;
  } 
  
}
