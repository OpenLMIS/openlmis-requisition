package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Service
public class SupervisoryNodeReferenceDataService
        extends BaseReferenceDataService<SupervisoryNodeDto> {

  @Override
  protected String getUrl() {
    return "/api/supervisoryNodes/";
  }

  @Override
  protected Class<SupervisoryNodeDto> getResultClass() {
    return SupervisoryNodeDto.class;
  }

  @Override
  protected Class<SupervisoryNodeDto[]> getArrayResultClass() {
    return SupervisoryNodeDto[].class;
  }

  /**
   * Find a correct supervisor node by the provided facility and program.
   *
   */
  public SupervisoryNodeDto findSupervisorNode(UUID program, UUID facility) {
    Map<String, Object> parameters = new HashMap<>();
    parameters.put("programId", program);
    parameters.put("facilityId", facility);

    List<SupervisoryNodeDto> supervisoryNodeDtos = new ArrayList<>(findAll("search", parameters));
    return supervisoryNodeDtos.isEmpty() ? null : supervisoryNodeDtos.get(0);
  }

}
