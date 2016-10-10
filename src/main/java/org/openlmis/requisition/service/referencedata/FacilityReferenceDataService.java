package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.FacilityDto;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
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
   *
   * @param code Field with string to find similar code.
   * @param name Filed with string to find similar name.
   * @return List of FacilityDtos with similar code or name.
   */
  public Collection<FacilityDto> searchFacilitiesBySimilarCodeOrName(String code, String name) {
    Map<String, Object> parameters = new HashMap<>();
    if (code != null) {
      parameters.put("code", code);
    }
    if (name != null) {
      parameters.put("name", name);
    }

    return findAll("findFacilitiesWithSimilarCodeOrName",parameters);
  }

  /**
   * Retrieves supply lines from reference data service by program and supervisory node.
   * @param programId UUID of the program
   * @param supervisoryNodeId UUID of the supervisory node
   * @return A list of supply lines matching search criteria
   * */
  public Collection<FacilityDto> searchSupplyingDepots(UUID programId, UUID supervisoryNodeId) {
    Map<String, Object> parameters = new HashMap<>();
    parameters.put("program", programId);
    parameters.put("supervisoryNode", supervisoryNodeId);

    return findAll("supplying", parameters);
  }
}
