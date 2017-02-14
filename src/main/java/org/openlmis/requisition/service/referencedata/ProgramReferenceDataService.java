package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.service.RequestParameters;
import org.springframework.stereotype.Service;

import java.util.Collection;

@Service
public class ProgramReferenceDataService extends BaseReferenceDataService<ProgramDto> {

  @Override
  protected String getUrl() {
    return "/api/programs/";
  }

  @Override
  protected Class<ProgramDto> getResultClass() {
    return ProgramDto.class;
  }

  @Override
  protected Class<ProgramDto[]> getArrayResultClass() {
    return ProgramDto[].class;
  }

  /**
   * This method retrieves Programs with programName similar with name parameter.
   *
   * @param programName Field with string to find similar name.
   * @return List of ProgramDtos with similar programName.
   */
  public Collection<ProgramDto> search(String programName) {
    return findAll("search", RequestParameters.init().set("name", programName));
  }
}
