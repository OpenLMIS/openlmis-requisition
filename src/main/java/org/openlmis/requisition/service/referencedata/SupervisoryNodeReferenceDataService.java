package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.springframework.stereotype.Service;

@Service
public class SupervisoryNodeReferenceDataService
        extends BaseReferenceDataService<SupervisoryNodeDto> {

  @Override
  protected String getUrl() {
    return "http://referencedata:8080/api/supervisoryNodes/";
  }

  @Override
  protected Class<SupervisoryNodeDto> getResultClass() {
    return SupervisoryNodeDto.class;
  }
}
