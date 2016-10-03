package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.FacilityTypeApprovedProductDto;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Service
public class FacilityTypeApprovedProductReferenceDataService
    extends BaseReferenceDataService<FacilityTypeApprovedProductDto> {
  @Override
  protected String getUrl() {
    return "/api/facilityTypeApprovedProducts/";
  }

  @Override
  protected Class<FacilityTypeApprovedProductDto> getResultClass() {
    return FacilityTypeApprovedProductDto.class;
  }

  @Override
  protected Class<FacilityTypeApprovedProductDto[]> getArrayResultClass() {
    return FacilityTypeApprovedProductDto[].class;
  }

  /**
   * Retrieves full supply FacilityTypeApprovedProduct from the reference data service by
   * facility id and program id.
   *
   * @param facilityId UUID of facility
   * @param programId UUID of program
   * @return A list of FacilityTypeApprovedProductDto matching search criteria
   */
  public Collection<FacilityTypeApprovedProductDto> getFullSupply(UUID facilityId,
                                                                  UUID programId) {
    Map<String, Object> parameters = new HashMap<>();
    parameters.put("facility", facilityId);
    parameters.put("program", programId);

    return findAll("search", parameters);
  }
}
