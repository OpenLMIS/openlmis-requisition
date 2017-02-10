package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.SupervisoryNodeDto;
import org.openlmis.requisition.service.RequestParameters;
import org.springframework.stereotype.Service;

import java.util.List;
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
   * Find a correct supervisory node by the provided facility and program.
   */
  public SupervisoryNodeDto findSupervisoryNode(UUID program, UUID facility) {
    RequestParameters parameters = RequestParameters
        .init()
        .set("programId", program)
        .set("facilityId", facility);

    List<SupervisoryNodeDto> supervisoryNodeDtos = findAll("search", parameters);
    return supervisoryNodeDtos.isEmpty() ? null : supervisoryNodeDtos.get(0);
  }

}
