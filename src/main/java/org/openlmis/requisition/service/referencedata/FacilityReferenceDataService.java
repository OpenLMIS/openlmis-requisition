package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.FacilityDto;
import org.springframework.stereotype.Service;

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
}
