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
import org.openlmis.requisition.domain.StatusChange;
import java.util.List;

public class BatchApproveRequisitionDto extends RequisitionDto {

  @JsonIgnore
  @Override
  public void setStatusChanges(List<StatusChange> statusChanges) {
    throw new IllegalStateException("The " + getClass().getName()
        + " does not provide status changes export");
  }

  @Override
  public boolean provideStatusChanges() {
    return false;
  }
}
