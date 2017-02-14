package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.ApprovedProductDto;
import org.openlmis.requisition.service.RequestParameters;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.UUID;

@Service
public class ApprovedProductReferenceDataService extends
    BaseReferenceDataService<ApprovedProductDto> {

  @Override
  protected String getUrl() {
    return "/api/facilities/";
  }

  @Override
  protected Class<ApprovedProductDto> getResultClass() {
    return ApprovedProductDto.class;
  }

  @Override
  protected Class<ApprovedProductDto[]> getArrayResultClass() {
    return ApprovedProductDto[].class;
  }

  /**
   * Retrieves all facility approved products from the reference data service, based on the
   * provided facility and full supply flag. It can be optionally filtered by the program ID.
   *
   * @param facilityId id of the facility
   * @param programId  id of the program
   * @param fullSupply whether the full supply or non-full supply products should be retrieved
   * @return a collection of approved products matching the search criteria
   */
  public Collection<ApprovedProductDto> getApprovedProducts(UUID facilityId, UUID programId,
                                                            boolean fullSupply) {
    RequestParameters parameters = RequestParameters
        .init()
        .set("programId", programId)
        .set("fullSupply", fullSupply);

    return findAll(facilityId + "/approvedProducts", parameters);
  }
}
