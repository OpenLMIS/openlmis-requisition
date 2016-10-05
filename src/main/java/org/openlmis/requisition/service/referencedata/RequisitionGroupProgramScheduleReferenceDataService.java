package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.RequisitionGroupProgramScheduleDto;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Service
public class RequisitionGroupProgramScheduleReferenceDataService
      extends BaseReferenceDataService<RequisitionGroupProgramScheduleDto> {

  @Override
  protected String getUrl() {
    return "/api/requisitionGroupProgramSchedules/";
  }

  @Override
  protected Class<RequisitionGroupProgramScheduleDto> getResultClass() {
    return RequisitionGroupProgramScheduleDto.class;
  }

  @Override
  protected Class<RequisitionGroupProgramScheduleDto[]> getArrayResultClass() {
    return RequisitionGroupProgramScheduleDto[].class;
  }

  /**
   * Retrieves requisition group program schedule from reference data service by program.
   * @param programId UUID of the program
   * @param facilityId UUID of the facility
   * @return Requisition Group Program Schedule matching search criteria
   */
  public Collection<RequisitionGroupProgramScheduleDto> searchByProgramAndFacility(
        UUID programId, UUID facilityId) {
    Map<String, Object> parameters = new HashMap<>();
    parameters.put("programId", programId);
    parameters.put("facilityId", facilityId);

    return findAll("search", parameters);
  }
}
