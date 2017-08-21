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

package org.openlmis.utils;

import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.UUID;

/**
 * Utility class for retrieving program from ReferenceData service
 * and checking if Date Physical Stock Count Completed is enabled.
 */
@Component
public class DatePhysicalStockCountCompletedEnabledPredicate {

  @Autowired
  private ProgramReferenceDataService programReferenceDataService;

  /**
   * Checks if program has Date Physical Stock Count Completed enabled.
   *
   * @param  programId requisition program ID
   * @return           if Date Physical Stock Count Completed is enabled
   */
  public boolean exec(UUID programId) {
    if (programId != null) {
      ProgramDto program = programReferenceDataService.findOne(programId);

      if (program != null && program.getEnableDatePhysicalStockCountCompleted() != null) {
        return program.getEnableDatePhysicalStockCountCompleted();
      }
    }

    return false;
  }
}
