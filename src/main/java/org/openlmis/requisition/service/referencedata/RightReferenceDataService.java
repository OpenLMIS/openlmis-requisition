package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.RightDto;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
public class RightReferenceDataService extends BaseReferenceDataService<RightDto> {

  @Override
  protected String getUrl() {
    return "/api/rights/";
  }

  @Override
  protected Class<RightDto> getResultClass() {
    return RightDto.class;
  }

  @Override
  protected Class<RightDto[]> getArrayResultClass() {
    return RightDto[].class;
  }

  /**
   * Find a correct right by the provided name.
   *
   * @param name right name
   * @return right related with the name or {@code null}.
   */
  public RightDto findRight(String name) {
    Map<String, Object> parameters = new HashMap<>();
    parameters.put("name", name);

    List<RightDto> rights = new ArrayList<>(findAll("search", parameters));
    return rights.size() > 0 ? rights.get(0) : null;
  }

}
