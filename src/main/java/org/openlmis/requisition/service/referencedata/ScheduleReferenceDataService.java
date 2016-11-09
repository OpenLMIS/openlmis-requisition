package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Service
public class ScheduleReferenceDataService extends BaseReferenceDataService<ProcessingScheduleDto> {

  @Override
  protected String getUrl() {
    return "/api/processingSchedules/";
  }

  @Override
  protected Class<ProcessingScheduleDto> getResultClass() {
    return ProcessingScheduleDto.class;
  }

  @Override
  protected Class<ProcessingScheduleDto[]> getArrayResultClass() {
    return ProcessingScheduleDto[].class;
  }

  /**
   * Retrieves schedule from reference data service by program and facility.
   * @param programId UUID of the program
   * @param facilityId UUID of the facility
   * @return schedule matching search criteria
   */
  public Collection<ProcessingScheduleDto> searchByProgramAndFacility(
      UUID programId, UUID facilityId) {
    Map<String, Object> parameters = new HashMap<>();
    parameters.put("programId", programId);
    parameters.put("facilityId", facilityId);

    return findAll("search", parameters);
  }
}
