package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.SupervisoryNodeDto;

/**
 * Created by user on 16.09.16.
 */
public class SupervisoryNodeReferenceDataService extends BaseReferenceDataService<SupervisoryNodeDto> {
  @Override
  protected String getUrl() {
    return "/supervisoryNodes/";
  }

  @Override
  protected Class<SupervisoryNodeDto> getResultClass() {
    return SupervisoryNodeDto.class;
  }
}
