package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.service.RequestParameters;
import org.springframework.stereotype.Service;

import java.util.Collection;
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
    return findAll(
        userId + "/supervisedFacilities",
        RequestParameters.init().set("rightId", rightId).set("programId", programId)
    );
  }
}
