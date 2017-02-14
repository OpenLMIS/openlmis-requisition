package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.service.RequestParameters;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.UUID;

@Service
public class SupervisedUsersReferenceDataService extends UserReferenceDataService {

  @Override
  protected String getUrl() {
    return "/api/supervisoryNodes/";
  }

  /**
   * Get a list of supervised users for a certain supervisory node, program and right.
   *
   * @param supervisoryNode the UUID of the supervisory node.
   * @param right the UUID of the right.
   * @param program the UUID of the program.
   * @return a collection of supervised users.
   */
  public Collection<UserDto> findAll(UUID supervisoryNode, UUID right, UUID program) {
    return findAll(supervisoryNode + "/supervisedUsers",
        RequestParameters.init().set("rightId", right).set("programId", program));
  }
}
