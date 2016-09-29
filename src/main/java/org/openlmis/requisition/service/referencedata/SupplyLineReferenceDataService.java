package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.SupplyLineDto;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Service
public class SupplyLineReferenceDataService extends BaseReferenceDataService<SupplyLineDto> {

  @Override
  protected String getUrl() {
    return "/api/supplyLines/";
  }

  @Override
  protected Class<SupplyLineDto> getResultClass() {
    return SupplyLineDto.class;
  }

  /**
   * Retrieves supply lines from reference data service by program and supervisory node.
   * @param programId UUID of the program
   * @param supervisoryNodeId UUID of the supervisory node
   * @return A list of supply lines matching search criteria
   */
  public Collection<SupplyLineDto> search(UUID programId, UUID supervisoryNodeId) {
    Map<String, Object> parameters = new HashMap<>();
    parameters.put("programId", programId);
    parameters.put("supervisoryNodeId", supervisoryNodeId);

    return findAll("searchByUUID", parameters);
  }
}
