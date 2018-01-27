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

import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.AvailableRequisitionColumnOption;

public class AvailableRequisitionColumnOptionDataBuilder {

  private AvailableRequisitionColumn requisitionColumn;
  private String optionName;
  private String optionLabel;

  /**
   * Builder for {@link AvailableRequisitionColumnOption}.
   */
  public AvailableRequisitionColumnOptionDataBuilder() {
    requisitionColumn = null;
    optionName = "option";
    optionLabel = "Option";
  }

  /**
   * Builds {@link AvailableRequisitionColumnOption} instance with test data.
   */
  public AvailableRequisitionColumnOption build() {
    return new AvailableRequisitionColumnOption(requisitionColumn, optionName, optionLabel);
  }
}
