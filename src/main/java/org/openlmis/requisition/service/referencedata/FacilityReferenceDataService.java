package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.service.RequestParameters;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.UUID;

@Service
public class FacilityReferenceDataService extends BaseReferenceDataService<FacilityDto> {

  @Override
  protected String getUrl() {
    return "/api/facilities/";
  }

  @Override
  protected Class<FacilityDto> getResultClass() {
    return FacilityDto.class;
  }

  @Override
  protected Class<FacilityDto[]> getArrayResultClass() {
    return FacilityDto[].class;
  }

  /**
   * This method retrieves Facilities with facilityName similar with name parameter or
   * facilityCode similar with code parameter.
   *
   * @param code Field with string to find similar code.
   * @param name Filed with string to find similar name.
   * @return List of FacilityDtos with similar code or name.
   */
  public Collection<FacilityDto> search(String code, String name) {
    RequestParameters parameters = RequestParameters
        .init()
        .set("code", code)
        .set("name", name);

    return findAll("search", parameters);
  }

  /**
   * Retrieves supply lines from reference data service by program and supervisory node.
   *
   * @param programId         UUID of the program
   * @param supervisoryNodeId UUID of the supervisory node
   * @return A list of supply lines matching search criteria
   */
  public Collection<FacilityDto> searchSupplyingDepots(UUID programId, UUID supervisoryNodeId) {
    RequestParameters parameters = RequestParameters
        .init()
        .set("programId", programId)
        .set("supervisoryNodeId", supervisoryNodeId);

    return findAll("supplying", parameters);
  }
}
