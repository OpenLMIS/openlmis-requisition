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

package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.service.RequestParameters;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.UUID;

@Service
public class UserSupervisedProgramsReferenceDataService extends ProgramReferenceDataService {

  @Override
  protected String getUrl() {
    return "/api/users/";
  }

  /**
   * Retrieves all the programs supervised by the given user from the reference data service.
   *
   * @param userUuid the UUID of the user
   * @return a collection of programs supervised by user
   */
  public Collection<ProgramDto> getProgramsSupervisedByUser(UUID userUuid) {
    return findAll(
        userUuid + "/programs",
        RequestParameters.init().set("forHomeFacility", false)
    );
  }

  /**
   * Retrieves all home facility programs for given user from the reference data
   * service.
   *
   * @param userUuid the UUID of the user
   * @return a collection of home facility programs for user
   */
  public Collection<ProgramDto> getHomeFacilityProgramsByUser(UUID userUuid) {
    return findAll(userUuid + "/programs");
  }
}
