package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.FacilityDto;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Service
public class UserSupervisedFacilitiesReferenceDataService extends FacilityReferenceDataService {

  @Override
  protected String getUrl() {
    return "/api/users/";
  }

  /**
   * Retrieves all the programs supervised by the given user from the reference data service.
   *
   * @param userId the UUID of the user
   * @param programId the UUID of the program
   * @param rightId the UUID of the right
   * @return a collection of facilities supervised by user
   */
  public Collection<FacilityDto> getFacilitiesSupervisedByUser(UUID userId, UUID programId,
                                                               UUID rightId) {
    Map<String, Object> parameters = new HashMap<>();
    parameters.put("rightId", rightId);
    parameters.put("programId", programId);

    return findAll(userId + "/supervisedFacilities", parameters);
  }
}
