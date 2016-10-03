package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.UserDto;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.Map;

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

  @Override
  protected Class<UserDto[]> getArrayResultClass() {
    return UserDto[].class;
  }

  public Collection<UserDto> findUsers(Map<String, Object> parameters) {
    return findAll("search", parameters);
  }
}
