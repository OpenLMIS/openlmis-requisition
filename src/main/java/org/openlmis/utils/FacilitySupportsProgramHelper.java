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

import static org.openlmis.requisition.i18n.MessageKeys.ERROR_FACILITY_DOES_NOT_SUPPORT_PROGRAM;

import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.SupportedProgramDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.List;
import java.util.UUID;

@Component
public class FacilitySupportsProgramHelper {

  @Autowired
  FacilityReferenceDataService facilityReferenceDataService;

  @Value("${time.zoneId}")
  private String timeZoneId;

  /**
   * Method check if facility supports program.
   *
   * @param facilityId facilityId Uuid of the Facility.
   * @param programId  programId UUID of the Program.
   */
  public void checkIfFacilitySupportsProgram(UUID facilityId, UUID programId) {
    FacilityDto facility = facilityReferenceDataService.findOne(facilityId);
    List<SupportedProgramDto> supportedPrograms = facility.getSupportedPrograms();

    if (!isProgramSupported(supportedPrograms, programId)) {
      throw new ValidationMessageException(
          new Message(ERROR_FACILITY_DOES_NOT_SUPPORT_PROGRAM, facilityId, programId));
    }
  }

  private boolean isProgramSupported(
      List<SupportedProgramDto> supportedPrograms, UUID programId) {
    return supportedPrograms.stream()
        .anyMatch(supportedProgram -> supportedProgram.getId().equals(programId)
            && supportedProgram.isSupportActive() && supportedProgram.isProgramActive()
            && isStartDateBeforeNow(supportedProgram.getSupportStartDate()));
  }

  private boolean isStartDateBeforeNow(LocalDate startDate) {
    return (startDate == null) || startDate.isBefore(getCurrentDateWithSystemZone());
  }

  private LocalDate getCurrentDateWithSystemZone() {
    return LocalDate.now(ZoneId.of(timeZoneId));
  }
}
