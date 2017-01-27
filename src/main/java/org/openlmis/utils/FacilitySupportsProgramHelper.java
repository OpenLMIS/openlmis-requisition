package org.openlmis.utils;

import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.SupportedProgramDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.List;
import java.util.UUID;

@Component
public class FacilitySupportsProgramHelper {
  public static final String REQUISITION_TIME_ZONE_ID = "requisition.time.zoneId";

  @Autowired
  FacilityReferenceDataService facilityReferenceDataService;

  @Autowired
  private ConfigurationSettingService configurationSettingService;

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
    return (startDate == null) || startDate.isBefore(getCurrentDateWithSystemZone());
  }

  private LocalDate getCurrentDateWithSystemZone() {
    String zone = configurationSettingService.getStringValue(REQUISITION_TIME_ZONE_ID);
    return LocalDate.now(ZoneId.of(zone));
  }
}
