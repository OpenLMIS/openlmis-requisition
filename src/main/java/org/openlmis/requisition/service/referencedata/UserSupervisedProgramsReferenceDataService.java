package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.ProgramDto;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Service
public class UserSupervisedProgramsReferenceDataService extends ProgramReferenceDataService {

  @Override
  protected String getUrl() {
    return "/api/users/";
  }

  /**
   * Retrieves all the programs supervised by the given user from the reference data service.
   * @param userUuid the UUID of the user
   * @return a collection of programs supervised by user
   */
  public Collection<ProgramDto> getProgramsSupervisedByUser(UUID userUuid) {
    Map<String, Object> searchParameters = new HashMap<>();
    searchParameters.put("forHomeFacility", false);
    return findAll(userUuid + "/programs", searchParameters);
  }
}
