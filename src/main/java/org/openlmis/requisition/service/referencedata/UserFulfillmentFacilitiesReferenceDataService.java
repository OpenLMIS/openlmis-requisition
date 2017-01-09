package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.FacilityDto;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.UUID;

@Service
public class UserFulfillmentFacilitiesReferenceDataService extends FacilityReferenceDataService {

  @Override
  protected String getUrl() {
    return "/api/users/";
  }

  /**
   * Retrieves all the facilities that the given user has fulfillment rights for.
   *
   * @param userUuid the UUID of the user
   * @return a collection of facilities the user has fulfillment rights for
   */
  public Collection<FacilityDto> getFulfillmentFacilities(UUID userUuid) {
    return findAll(userUuid + "/fulfillmentFacilities");
  }
}
