package org.openlmis.utils;

import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.SupportedProgramDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
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
   */
  public void checkIfFacilitySupportsProgram(UUID facilityId, UUID programId) {
    FacilityDto facility = facilityReferenceDataService.findOne(facilityId);
    List<SupportedProgramDto> supportedPrograms = facility.getSupportedPrograms();

    if (!isProgramSupported(supportedPrograms, programId)) {
      throw new ValidationMessageException(new Message(
          "requisition.error.facility-does-not-support-program", facilityId, programId));
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
    return (startDate == null) || startDate.isBefore(getCurrentUtcDate());
  }

  private LocalDate getCurrentUtcDate() {
    return LocalDate.now();
  }
}
