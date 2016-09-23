package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.UserDto;
import org.springframework.stereotype.Service;

@Service
public class UserReferenceDataService extends BaseReferenceDataService<UserDto> {

  @Override
  protected String getUrl() {
    return "http://referencedata:8080/api/users/";
  }

  @Override
  protected Class<UserDto> getResultClass() {
    return UserDto.class;
  }
}
