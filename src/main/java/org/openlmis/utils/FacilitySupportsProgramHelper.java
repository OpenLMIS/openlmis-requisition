package org.openlmis.utils;

import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.SupportedProgramDto;
import org.openlmis.requisition.exception.FacilityNotSupportsProgramException;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.UUID;

@Component
public class FacilitySupportsProgramHelper {

  @Autowired
  FacilityReferenceDataService facilityReferenceDataService;

  /**
   * Method check if facility supports program.
   *
   * @param facilityId facilityId Uuid of the Facility.
   * @param programId  programId UUID of the Program.
   * @throws FacilityNotSupportsProgramException if facility not supports program.
   */
  public void checkIfFacilitySupportsProgram(UUID facilityId, UUID programId)
      throws FacilityNotSupportsProgramException {
    FacilityDto facility = facilityReferenceDataService.findOne(facilityId);
    List<SupportedProgramDto> supportedPrograms = facility.getSupportedPrograms();

    if (!contains(supportedPrograms, programId)) {
      throw new FacilityNotSupportsProgramException("Facility with id " + facilityId
          + " not supports program with id " + programId);
    }
  }

  private boolean contains(List<SupportedProgramDto> supportedPrograms, UUID programId) {
    return supportedPrograms.stream()
        .anyMatch(supportedProgram -> supportedProgram.getId().equals(programId)
            && supportedProgram.isSupportActive() && supportedProgram.isProgramActive()
            && checkDate(supportedProgram));
  }

  private boolean checkDate(SupportedProgramDto supportedProgram) {
    ZonedDateTime zonedStartDate = supportedProgram.getZonedStartDate();
    return (zonedStartDate == null) || !getCurrentDate().isBefore(zonedStartDate);
  }

  private ZonedDateTime getCurrentDate() {
    return ZonedDateTime.of(LocalDate.now(), LocalTime.MIDNIGHT, ZoneId.of("UTC"));
  }
}
