package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.UserDto;
import org.springframework.stereotype.Service;

@Service
public class UserReferenceDataService extends BaseReferenceDataService<UserDto> {

  @Override
  protected String getUrl() {
    return "/api/users/";
  }

  @Override
  protected Class<UserDto> getResultClass() {
    return UserDto.class;
  }
}
