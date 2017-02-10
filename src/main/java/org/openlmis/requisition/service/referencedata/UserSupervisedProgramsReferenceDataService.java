package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.service.RequestParameters;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.UUID;

@Service
public class UserSupervisedProgramsReferenceDataService extends ProgramReferenceDataService {

  @Override
  protected String getUrl() {
    return "/api/users/";
  }

  /**
   * Retrieves all the programs supervised by the given user from the reference data service.
   *
   * @param userUuid the UUID of the user
   * @return a collection of programs supervised by user
   */
  public Collection<ProgramDto> getProgramsSupervisedByUser(UUID userUuid) {
    return findAll(
        userUuid + "/programs",
        RequestParameters.init().set("forHomeFacility", false)
    );
  }

  /**
   * Retrieves all home facility programs for given user from the reference data
   * service.
   *
   * @param userUuid the UUID of the user
   * @return a collection of home facility programs for user
   */
  public Collection<ProgramDto> getHomeFacilityProgramsByUser(UUID userUuid) {
    return findAll(userUuid + "/programs");
  }
}
